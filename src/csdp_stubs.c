#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <stdlib.h>

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

static void build_obj(value ml_obj, struct blockmatrix **obj)
{
  int nb_blocks, sz, i, j, k;
  value lit, matrix, line;
  struct blockrec *b;

  nb_blocks = list_length(ml_obj);

  *obj = create_blockmatrix(nb_blocks);
  k = 0;
  for (lit = ml_obj; lit != Val_emptylist; lit = Field(lit, 1)) {
    ++k;
    matrix = Field(lit, 0);
    sz = Wosize_val(matrix);
    b = &((*obj)->blocks[k]);
    b->blocksize = sz;
    b->data.mat = (double*)malloc_fail(sz * sz, sizeof(double));
    for (i = 0; i < sz; ++i) {
      line = Field(matrix, i);
      for (j = 0; j < sz; ++j)
        b->data.mat[ijtok(i + 1, j + 1, sz)] = Double_field(line, j);
    }
  }
}

static void build_sparseblockmatrix(value ml_bm, int constr_num,
                                    struct sparseblock **sp)
{
  int nb_entries, i, j, k, m, sz;
  value lit, matrix, line;
  struct sparseblock **prev = sp;
  struct sparseblock *b;
  double e;

  k = 0;
  for (lit = ml_bm; lit != Val_emptylist; lit = Field(lit, 1)) {
    ++k;
    matrix = Field(lit, 0);
    sz = Wosize_val(matrix);
    nb_entries = 0;
    for (i = 0; i < sz; ++i) {
      line = Field(matrix, i);
      for (j = i; j < sz; ++j) if (Double_field(line, j) != 0.) ++nb_entries;
    }
    b = create_sparseblock(constr_num, k, sz, nb_entries);
    m = 0;
    for (i = 0; i < sz; ++i) {
      line = Field(matrix, i);
      for (j = i; j < sz; ++j) {
        e = Double_field(line, j);
        if (e != 0.) {
          ++m;
          b->entries[m] = e;
          b->iindices[m] = i + 1;
          b->jindices[m] = j + 1;
        }
      }
    }
    *prev = b;
    prev = &(b->next);
  }
}

static void build_cstrs(value ml_cstrs, int *nb_cstrs,
                        struct constraintmatrix **cstrs, double **b)
{
  int k;
  value lit, constraint;

  *nb_cstrs = list_length(ml_cstrs);

  *b = create_vector(*nb_cstrs);
  *cstrs = create_constraintmatrices(*nb_cstrs);
  k = 0;
  for (lit = ml_cstrs; lit != Val_emptylist; lit = Field(lit, 1)) {
    ++k;
    constraint = Field(lit, 0);
    (*b)[k] = Double_val(Field(constraint, 1));
    build_sparseblockmatrix(Field(constraint, 0), k, &((*cstrs)[k].blocks));
  }
}

static void solve(int dim_X, int nb_cstrs, struct blockmatrix *obj,
                  double *b, struct constraintmatrix *cstrs,
                  sdp_ret_t *sdp_ret, double *pobj, double *dobj,
                  struct blockmatrix *res_X, double **res_y)
{
  struct blockmatrix X,Z;
  double *y;
  int i, ret;

  /* write_prob("prob.prob", dim_X, nb_cstrs, *obj, b, cstrs); */

  initsoln(dim_X, nb_cstrs, *obj, b, cstrs, &X, &y, &Z);
  ret = easy_sdp(dim_X, nb_cstrs, *obj, b, cstrs, 0.0,
                 &X, &y, &Z, pobj, dobj);
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
  case 4:
    *sdp_ret = SDP_RET_MAX_ITER_REACHED;
    break;
  case 5:  /* stuck at edge of primal infeasibility */
    *sdp_ret = SDP_RET_NEAR_PRIMAL_INFEASIBLE;
    break;
  case 6:  /* stuck at edge of dual infeasibility */
    *sdp_ret = SDP_RET_NEAR_DUAL_INFEASIBLE;
    break;
  case 7:  /* lack of progress */
    *sdp_ret = SDP_RET_LACK_OF_PROGRESS;
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

value csdp_solve(value ml_obj, value ml_cstrs)
{
  CAMLparam2(ml_obj, ml_cstrs);

  CAMLlocal5(ml_res, ml_res_obj, ml_res_Xy, ml_res_X, ml_res_y);
  CAMLlocal3(cons, matrix, line);

  value lit;
  struct blockmatrix *obj, res_X;
  int nb_cstrs, dim_X;
  struct constraintmatrix *cstrs;
  double *b, pobj, dobj, *res_y;
  sdp_ret_t sdp_ret;

  dim_X = 0;
  for (lit = ml_obj; lit != Val_emptylist; lit = Field(lit, 1))
    dim_X += Wosize_val(Field(lit, 0));

  build_obj(ml_obj, &obj);

  build_cstrs(ml_cstrs, &nb_cstrs, &cstrs, &b);
  solve(dim_X, nb_cstrs, obj, b, cstrs, &sdp_ret, &pobj, &dobj, &res_X, &res_y);
  build_res_X(&res_X, &ml_res_X, &cons, &matrix, &line);
  build_res_y(nb_cstrs, res_y, &ml_res_y);

  /* TODO: free res_X and res_y */

  ml_res = caml_alloc(3, 0);

  Store_field(ml_res, 0, Val_int(sdp_ret));
  ml_res_obj = caml_alloc(2, 0);
  Store_field(ml_res_obj, 0, caml_copy_double(pobj));
  Store_field(ml_res_obj, 1, caml_copy_double(dobj));
  Store_field(ml_res, 1, ml_res_obj);
  ml_res_Xy = caml_alloc(2, 0);
  Store_field(ml_res_Xy, 0, ml_res_X);
  Store_field(ml_res_Xy, 1, ml_res_y);
  Store_field(ml_res, 2, ml_res_Xy);

  CAMLreturn(ml_res);
}

#else

value csdp_solve(value ml_obj, value ml_cstrs)
{
  CAMLparam2(ml_obj, ml_cstrs);

  CAMLlocal1(ml_res);

  caml_failwith("caml_osdp: compiled without CSDP support!");

  CAMLreturn(ml_res);
}

#endif  /* WITH_CSDP_yes */
