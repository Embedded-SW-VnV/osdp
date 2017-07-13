/*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <stdlib.h>

/* for fflush (flush CSDP debug output) before turning hand back to Caml
   (avoid arbitrary mixing CSDP output with OCaml Format output) */
#include <stdio.h>

#include "config.h"
#ifdef WITH_CSDP_yes

#include "declarations.h"
#include "sdp_ret.h"

static void *malloc_fail(int nb, int size)
{
  void *p;

  p = malloc(nb * size);
  if (p == NULL) caml_failwith("caml_osdp: out of memory!");

  return p;
}

static void collect_matrix_indexes(value mx,
                                   int *min, int *max)
{
  value tmp;
  int i;
  
  for (tmp = mx; tmp != Val_emptylist; tmp = Field(tmp, 1)) {
    i = Int_val(Field(Field(tmp, 0), 0));
    if (i < *min) *min = i;
    if (i > *max) *max = i;
  }
}

static void collect_matrix_sizes(value mx, int idx_offset, int *dimvar)
{
  value tmp, tmp2;
  int i, j;
  
  for (tmp = mx; tmp != Val_emptylist; tmp = Field(tmp, 1)) {
    i = Int_val(Field(Field(tmp, 0), 0)) - idx_offset;
    for (tmp2 = Field(Field(tmp, 0), 1);
         tmp2 != Val_emptylist; tmp2 = Field(tmp2, 1)) {
      j = Int_val(Field(Field(tmp2, 0), 0));
      if (j >= dimvar[i]) dimvar[i] = j + 1;
      j = Int_val(Field(Field(tmp2, 0), 1));
      if (j >= dimvar[i]) dimvar[i] = j + 1;
    }
  }
}

static void collect_sizes(value ml_obj, value ml_cstrs,
                          int *idx_offset, int *nb_vars, int *nb_cstrs,
                          int **dimvar, int *dim_X)
{
  value tmp;
  int i;
  int min, max;
  
  min = 2147483647;
  max = -2147483647;
  
  collect_matrix_indexes(ml_obj, &min, &max);

  *nb_cstrs = 0;
  for (tmp = ml_cstrs; tmp != Val_emptylist; tmp = Field(tmp, 1)) {
    collect_matrix_indexes(Field(Field(tmp, 0), 0), &min, &max);
    ++(*nb_cstrs);
  }

  *idx_offset = min;
  *nb_vars = max - min + 1;

  *dimvar = (int*)malloc_fail(*nb_vars, sizeof(int));

  for (i = 0; i < *nb_vars; ++i) (*dimvar)[i] = 0;
  
  collect_matrix_sizes(ml_obj, *idx_offset, *dimvar);

  for (tmp = ml_cstrs; tmp != Val_emptylist; tmp = Field(tmp, 1)) {
    collect_matrix_sizes(Field(Field(tmp, 0), 0), *idx_offset, *dimvar);
  }

  *dim_X = 0;
  for (i = 0; i < *nb_vars; ++i) *dim_X += (*dimvar)[i];
}

static struct blockmatrix *create_blockmatrix(int n)
{
  struct blockmatrix *b;
  int k;

  b = (struct blockmatrix*)malloc_fail(1, sizeof(struct blockmatrix));
  b->nblocks = n;
  b->blocks = (struct blockrec*)malloc_fail(n + 1, sizeof(struct blockrec));
  for (k = 1; k <= n; ++k) {
    b->blocks[k].blockcategory = MATRIX;
    b->blocks[k].blocksize = 0;
  }

  return b;
}

static struct constraintmatrix *create_constraintmatrices(int n)
{
  struct constraintmatrix *cms;
  int k;

  cms = (struct constraintmatrix*)malloc_fail(n + 1,
                                              sizeof(struct constraintmatrix));
  for (k = 1; k <= n; ++k) {
    cms[k].blocks = NULL;
  }

  return cms;
}

static double *create_vector(int n)
{
  double *v;
  int k;

  v = (double*)malloc_fail(n + 1, sizeof(double));
  for (k = 1; k <= n; ++k) v[k] = 0.;

  return v;
}

static struct sparseblock *create_sparseblock(int constraint_num,
                                              int block_num,
                                              int blocksize,
                                              int nb_entries)
{
  struct sparseblock * b;

  b = (struct sparseblock*)malloc_fail(1, sizeof(struct sparseblock));
  b->blocknum = block_num;
  b->blocksize = blocksize;
  b->constraintnum = constraint_num;
  b->next = NULL;
  b->nextbyblock = NULL;
  b->numentries = nb_entries;
  b->entries = (double*)malloc_fail(nb_entries + 1, sizeof(double));
  b->iindices = (int*)malloc_fail(nb_entries + 1, sizeof(int));
  b->jindices = (int*)malloc_fail(nb_entries + 1, sizeof(int));

  return b;
}

static int list_length(value list)
{
  value tmp;
  int cpt;
  
  cpt = 0;
  for (tmp = list; tmp != Val_emptylist; tmp = Field(tmp, 1)) ++cpt;

  return cpt;
}

static void build_obj(value ml_obj, int idx_offset, int nb_vars, int *dimvar,
                      struct blockmatrix **obj)
{
  int sz, i, j, k;
  value lit, matrix, elt;
  struct blockrec *b;
  double e;

  *obj = create_blockmatrix(nb_vars);
  for (k = 1; k <= nb_vars; ++k) {
    sz = dimvar[k - 1];
    b = &((*obj)->blocks[k]);
    b->blocksize = sz;
    b->data.mat = (double*)malloc_fail(sz * sz, sizeof(double));
    for (i = 0; i < sz; ++i)
      for (j = 0; j < sz; ++j)
        b->data.mat[ijtok(i + 1, j + 1, sz)] = 0;
  }

  for (lit = ml_obj; lit != Val_emptylist; lit = Field(lit, 1)) {
    k = Int_val(Field(Field(lit, 0), 0)) - idx_offset + 1;
    b = &((*obj)->blocks[k]);
    sz = dimvar[k - 1];
    matrix = Field(Field(lit, 0), 1);
    for (elt = matrix; elt != Val_emptylist; elt = Field(elt, 1)) {
      i = Int_val(Field(Field(elt, 0), 0));
      j = Int_val(Field(Field(elt, 0), 1));
      e = Double_val(Field(Field(elt, 0), 2));
      b->data.mat[ijtok(i + 1, j + 1, sz)] = e;
      b->data.mat[ijtok(j + 1, i + 1, sz)] = e;
    }
  }
}

static void build_sparseblockmatrix(value ml_bm, int constr_num,
                                    int idx_offset, int *dimvar,
                                    struct sparseblock **sp)
{
  int nb_entries, i, j, k, m, sz;
  value lit, matrix, elt;
  struct sparseblock **prev = sp;
  struct sparseblock *b;
  double e;

  for (lit = ml_bm; lit != Val_emptylist; lit = Field(lit, 1)) {
    k = Int_val(Field(Field(lit, 0), 0)) - idx_offset + 1;
    sz = dimvar[k - 1];
    matrix = Field(Field(lit, 0), 1);
    nb_entries = list_length(matrix);
    b = create_sparseblock(constr_num, k, sz, nb_entries);
    m = 0;
    for (elt = matrix; elt != Val_emptylist; elt = Field(elt, 1)) {
      ++m;
      i = Int_val(Field(Field(elt, 0), 0));
      j = Int_val(Field(Field(elt, 0), 1));
      e = Double_val(Field(Field(elt, 0), 2));
      b->iindices[m] = j + 1;  /* lower triangular (j <= i) given as input */
      b->jindices[m] = i + 1;  /* but we need upper triangular, so transpose */
      b->entries[m] = e;
    }
    *prev = b;
    prev = &(b->next);
  }
}

static void build_cstrs(value ml_cstrs,
                        int idx_offset, int *dimvar, int nb_cstrs,
                        struct constraintmatrix **cstrs, double **b)
{
  int k;
  value lit, constraint;

  *b = create_vector(nb_cstrs);
  *cstrs = create_constraintmatrices(nb_cstrs);
  k = 0;
  for (lit = ml_cstrs; lit != Val_emptylist; lit = Field(lit, 1)) {
    ++k;
    constraint = Field(lit, 0);
    (*b)[k] = Double_val(Field(constraint, 1));
    build_sparseblockmatrix(Field(constraint, 0), k, idx_offset, dimvar,
                            &((*cstrs)[k].blocks));
  }
}

static void solve(int dim_X, int nb_cstrs, struct blockmatrix *obj,
                  double *b, struct constraintmatrix *cstrs,
                  sdp_ret_t *sdp_ret, double *pobj, double *dobj,
                  struct blockmatrix *res_X,
                  double **res_y, struct blockmatrix *res_Z,
                  int printlevel)
{
  struct blockmatrix X,Z;
  double *y;
  int i, ret;

  /* write_prob("prob.prob", dim_X, nb_cstrs, *obj, b, cstrs); */

  initsoln(dim_X, nb_cstrs, *obj, b, cstrs, &X, &y, &Z);
  ret = easy_sdp_params(dim_X, nb_cstrs, *obj, b, cstrs, 0.0,
                        &X, &y, &Z, pobj, dobj,
                        NULL, printlevel);
  switch (ret) {
  case 0:
    *sdp_ret = SDP_RET_SUCCESS;
    break;
  case 3:
    *sdp_ret = SDP_RET_PARTIAL_SUCCESS;
    break;
  case 1:
    *sdp_ret = SDP_RET_PRIMAL_INFEASIBLE;
    break;
  case 2:
    *sdp_ret = SDP_RET_DUAL_INFEASIBLE;
    break;
  case 4:  /* max iter reached */
    *sdp_ret = SDP_RET_PARTIAL_SUCCESS;
    break;
  case 5:  /* stuck at edge of primal infeasibility */
    *sdp_ret = SDP_RET_NEAR_PRIMAL_INFEASIBLE;
    break;
  case 6:  /* stuck at edge of dual infeasibility */
    *sdp_ret = SDP_RET_NEAR_DUAL_INFEASIBLE;
    break;
  case 7:  /* lack of progress */
    *sdp_ret = SDP_RET_PARTIAL_SUCCESS;
    break;
  case 8:  /* X, Z or O was singular */
  case 9:  /* detected NaN or Inf values */
  default:
    *sdp_ret = SDP_RET_UNKNOWN;
    break;
  }

  /* write_sol("prob.sol", dim_X, nb_cstrs, X, y, Z); */

  alloc_mat(X, res_X);
  copy_mat(X, *res_X);

  *res_y = (double*)malloc_fail(nb_cstrs, sizeof(double));
  for (i = 0; i < nb_cstrs; ++i) (*res_y)[i] = y[i + 1];

  alloc_mat(Z, res_Z);
  copy_mat(Z, *res_Z);

  free_prob(dim_X, nb_cstrs, *obj, b, cstrs, X, y, Z);
}

static void build_res_X(struct blockmatrix *res_X, value *ml_res_X,
                        value *cons, value *matrix, value *line)
{
  int i, j, k, sz;
  struct blockrec *b;

  *ml_res_X = Val_emptylist;
  for (k = res_X->nblocks; k >= 1; --k) {
    b = &(res_X->blocks[k]);
    sz = b->blocksize;
    *matrix = caml_alloc(sz, 0);
    switch (b->blockcategory) {
    case DIAG:
      for (i = 0; i < sz; ++i) {
        *line = caml_alloc(sz * Double_wosize, Double_array_tag);
        for (j = 0; j < i; ++j) Store_double_field(*line, j, 0.);
        Store_double_field(*line, j, b->data.vec[j + 1]);
        for (; j < sz; ++j) Store_double_field(*line, j, 0.);
        Store_field(*matrix, i, *line);
      }
      break;
    case MATRIX:
      for (i = 0; i < sz; ++i) {
        *line = caml_alloc(sz * Double_wosize, Double_array_tag);
        for (j = 0; j < sz; ++j)
          Store_double_field(*line, j, b->data.mat[ijtok(i + 1, j + 1, sz)]);
        Store_field(*matrix, i, *line);
      }
      break;
    case PACKEDMATRIX:
      caml_failwith("caml_osdp: unexpected packed matrix in result!");
      break;
    }
    *cons = caml_alloc(2, 0);
    Store_field(*cons, 0, *matrix);
    Store_field(*cons, 1, *ml_res_X);
    *ml_res_X = *cons;
  }
}

static void build_res_y(int nb_cstrs, double *res_y, value *ml_res_y)
{
  int i;

  *ml_res_y = caml_alloc(nb_cstrs * Double_wosize, Double_array_tag);
  for (i = 0; i < nb_cstrs; ++i) {
    Store_double_field(*ml_res_y, i, res_y[i]);
  }
}

value csdp_solve(value ml_printlevel, value ml_obj, value ml_cstrs)
{
  CAMLparam3(ml_printlevel, ml_obj, ml_cstrs);

  CAMLlocal2(ml_res, ml_res_obj);
  CAMLlocal4(ml_res_XyZ, ml_res_X, ml_res_y, ml_res_Z);
  CAMLlocal3(cons, matrix, line);

  struct blockmatrix *obj, res_X, res_Z;
  int idx_offset, nb_vars, *dimvar;
  int nb_cstrs, dim_X;
  struct constraintmatrix *cstrs;
  double *b, pobj, dobj, *res_y;
  sdp_ret_t sdp_ret;

  collect_sizes(ml_obj, ml_cstrs,
                &idx_offset, &nb_vars, &nb_cstrs, &dimvar, &dim_X);

  build_obj(ml_obj, idx_offset, nb_vars, dimvar, &obj);

  build_cstrs(ml_cstrs, idx_offset, dimvar, nb_cstrs, &cstrs, &b);
  solve(dim_X, nb_cstrs, obj, b, cstrs,
        &sdp_ret, &pobj, &dobj, &res_X, &res_y, &res_Z,
        Int_val(ml_printlevel));
  build_res_X(&res_X, &ml_res_X, &cons, &matrix, &line);
  build_res_y(nb_cstrs, res_y, &ml_res_y);
  build_res_X(&res_Z, &ml_res_Z, &cons, &matrix, &line);

  /* TODO: free res_X and res_y */

  free(dimvar);
  
  ml_res = caml_alloc(3, 0);

  Store_field(ml_res, 0, Val_int(sdp_ret));
  ml_res_obj = caml_alloc(2, 0);
  Store_field(ml_res_obj, 0, caml_copy_double(pobj));
  Store_field(ml_res_obj, 1, caml_copy_double(dobj));
  Store_field(ml_res, 1, ml_res_obj);
  ml_res_XyZ = caml_alloc(3, 0);
  Store_field(ml_res_XyZ, 0, ml_res_X);
  Store_field(ml_res_XyZ, 1, ml_res_y);
  Store_field(ml_res_XyZ, 2, ml_res_Z);
  Store_field(ml_res, 2, ml_res_XyZ);

  fflush(stdout);
  
  CAMLreturn(ml_res);
}

#else

value csdp_solve(value ml_printlevel, value ml_obj, value ml_cstrs)
{
  CAMLparam3(ml_printlevel, ml_obj, ml_cstrs);

  CAMLlocal1(ml_res);

  caml_failwith("caml_osdp: compiled without CSDP support!");

  CAMLreturn(ml_res);
}

#endif  /* WITH_CSDP_yes */
