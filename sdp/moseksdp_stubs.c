#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <stdlib.h>
#include <stdio.h>

#include "sdp_ret.h"
#include "mosek.h"

#define MS(c)                                   \
  do {                                          \
    if (r == MSK_RES_OK) r = (MSK_##c);         \
  } while (0)

static void *malloc_fail(int nb, int size)
{
  void *p;

  p = malloc(nb * size);
  if (p == NULL) caml_failwith("caml_csdp: out of memory!");

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
                          int **dimvar)
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
}

static int list_length(value list)
{
  value tmp;
  int cpt;
  
  cpt = 0;
  for (tmp = list; tmp != Val_emptylist; tmp = Field(tmp, 1)) ++cpt;

  return cpt;
}

static MSKrescodee MSK_read_sparsesymmat(MSKtask_t task, value mx, int dim,
                                         MSKint64t *idx)
{
  MSKrescodee r = MSK_RES_OK;
  value tmp;
  int i, sz;
  int *ai, *aj;
  double *av;
  
  sz = list_length(mx);
  
  ai = (int*)malloc_fail(sz, sizeof(int));
  aj = (int*)malloc_fail(sz, sizeof(int));
  av = (double*)malloc_fail(sz, sizeof(double));
  
  for (tmp = mx, i = 0; tmp != Val_emptylist; tmp = Field(tmp, 1), ++i) {
    ai[i] = Int_val(Field(Field(tmp, 0), 0));
    aj[i] = Int_val(Field(Field(tmp, 0), 1));
    av[i] = Double_val(Field(Field(tmp, 0), 2));
  }

  /* printf("ai = [%d", ai[0]); */
  /* for (i = 1; i < sz; ++i) printf(", %d", ai[i]); */
  /* printf("]\n"); */
  /* printf("aj = [%d", aj[0]); */
  /* for (i = 1; i < sz; ++i) printf(", %d", aj[i]); */
  /* printf("]\n"); */
  /* printf("av = [%g", av[0]); */
  /* for (i = 1; i < sz; ++i) printf(", %g", av[i]); */
  /* printf("]\n"); */

  MS(appendsparsesymmat(task, dim, sz, ai, aj, av, idx));

  free(av);
  free(aj);
  free(ai);

  return r;
}

static MSKrescodee MSK_read_barxj(MSKtask_t task, int j, int dim, value *matrix)
{
  MSKrescodee r = MSK_RES_OK;
  value line;
  double *barx;
  int i, k;

  barx = (double*)MSK_calloctask(task, dim * (dim + 1) / 2, sizeof(MSKrealt));

  MS(getbarxj(task, MSK_SOL_ITR, j, barx));
  
  *matrix = caml_alloc(dim, 0);
  for (i = 0; i < dim; ++i) {
    line = caml_alloc(dim * Double_wosize, Double_array_tag);
    Store_field(*matrix, i, line);
  }

  for (j = 0, k = 0; j < dim; ++j) {
    for (i = j; i < dim; ++i, ++k) {
      Store_double_field(Field(*matrix, i), j, barx[k]);
      Store_double_field(Field(*matrix, j), i, barx[k]);
    }
  }
  
  MSK_freetask(task, barx);

  return r;
}

static MSKrescodee MSK_read_barx(MSKtask_t task, int nb_vars, int *dimvar,
                                 value *matrix,
                                 value *ml_res_X)
{
  MSKrescodee r = MSK_RES_OK;
  value cons;
  int j;

  *ml_res_X = Val_emptylist;
  for (j = nb_vars - 1; j >= 0; --j) {
    MS(read_barxj(task, j, dimvar[j], matrix));
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, *matrix);
    Store_field(cons, 1, *ml_res_X);
    *ml_res_X = cons;
  }

  return r;
}

static MSKrescodee MSK_read_y(MSKtask_t task, int nb_cstrs, value *ml_res_y)
{
  MSKrescodee r = MSK_RES_OK;
  double *y;
  int i;

  y = (double*)MSK_calloctask(task, nb_cstrs, sizeof(MSKrealt));

  MS(gety(task, MSK_SOL_ITR, y));
              
  *ml_res_y = caml_alloc(nb_cstrs * Double_wosize, Double_array_tag);
  for (i = 0; i < nb_cstrs; ++i) {
    Store_double_field(*ml_res_y, i, -y[i]);
  }
  
  MSK_freetask(task, y);

  return r;
}

static void MSKAPI printstr(void *handle, MSKCONST char str[])
{
  printf("%s", str);
}

value moseksdp_solve(value ml_obj, value ml_cstrs)
{
  CAMLparam2(ml_obj, ml_cstrs);

  CAMLlocal5(ml_res, ml_res_obj, ml_res_Xy, ml_res_X, ml_res_y);
  CAMLlocal3(cons, matrix, line);

  value tmp, tmp2;
  int i, j;
  int idx_offset, nb_vars, nb_cstrs, *dimvar;
  MSKrescodee r = MSK_RES_OK;
  MSKenv_t env = NULL;
  MSKtask_t task = NULL;
  MSKsolstae solsta;
  MSKint64t idx;
  double falpha;
  double pobj, dobj;
  char symname[MSK_MAX_STR_LEN];
  char desc[MSK_MAX_STR_LEN];
  sdp_ret_t sdp_ret;
        
  collect_sizes(ml_obj, ml_cstrs, &idx_offset, &nb_vars, &nb_cstrs, &dimvar);

  /* printf("idx_offset = %d, nb_vars = %d, nb_cstrs = %d\n", */
  /*        idx_offset, nb_vars, nb_cstrs); */

  /* for (i = 0; i < nb_vars; ++i) { */
  /*   printf("dimvar[%d] = %d\n", i, dimvar[i]); */
  /* } */

  MS(makeenv(&env, NULL));
  MS(maketask(env, nb_cstrs, 0, &task));
  /* MS(linkfunctotaskstream(task, MSK_STREAM_LOG, NULL, printstr)); */
  MS(appendcons(task, nb_cstrs));
  MS(appendbarvars(task, nb_vars, dimvar));

  falpha = -1.0;
  for (tmp = ml_obj; tmp != Val_emptylist; tmp = Field(tmp, 1)) {
    j = Int_val(Field(Field(tmp, 0), 0));
    /* printf("C_%d\n", j); */
    MS(read_sparsesymmat(task, Field(Field(tmp, 0), 1), dimvar[j], &idx));
    MS(putbarcj(task, j, 1, &idx, &falpha));
  }

  falpha = 1.0;
  for (tmp = ml_cstrs, i = 0; tmp != Val_emptylist; tmp = Field(tmp, 1), ++i) {
    for (tmp2 = Field(Field(tmp, 0), 0);
         tmp2 != Val_emptylist; tmp2 = Field(tmp2, 1)) {
      j = Int_val(Field(Field(tmp2, 0), 0));
      /* printf("A_%d,%d\n", i, j); */
      MS(read_sparsesymmat(task, Field(Field(tmp2, 0), 1), dimvar[j], &idx));
      MS(putbaraij(task, i, j, 1, &idx, &falpha));
    }
    MS(putconbound(task, i, MSK_BK_FX,
                   Double_val(Field(Field(tmp, 0), 1)),
                   Double_val(Field(Field(tmp, 0), 1))));
  }

  MS(optimizetrm(task, NULL));

  /* Print a summary containing information
     about the solution for debugging purposes*/
  MS(solutionsummary(task, MSK_STREAM_MSG));
        
  MS(getsolsta(task, MSK_SOL_ITR, &solsta));

  /* default values */
  pobj = dobj = 0;
  ml_res_X = Val_emptylist;
  ml_res_y = Atom(Double_array_tag);

  if (solsta == MSK_SOL_STA_OPTIMAL || solsta == MSK_SOL_STA_NEAR_OPTIMAL) {
    /* printf("Optimal primal solution\n"); */
    MS(getprimalobj(task, MSK_SOL_ITR, &pobj));
    MS(getdualobj(task, MSK_SOL_ITR, &dobj));
    /* printf("pobj, dobj, res: % e, % e, % e\n", pobj, dobj, *res); */
    MS(read_barx(task, nb_vars, dimvar, &matrix, &ml_res_X));
    MS(read_y(task, nb_cstrs, &ml_res_y));
  }

  switch(solsta) {
  case MSK_SOL_STA_OPTIMAL:
    sdp_ret = SDP_RET_SUCCESS;
    break;
  case MSK_SOL_STA_NEAR_OPTIMAL:
    sdp_ret = SDP_RET_PARTIAL_SUCCESS;
    break;
  case MSK_SOL_STA_PRIM_INFEAS_CER:
    sdp_ret = SDP_RET_PRIMAL_INFEASIBLE;
    break;
  case MSK_SOL_STA_DUAL_INFEAS_CER:
    sdp_ret = SDP_RET_DUAL_INFEASIBLE;
    break;
  case MSK_SOL_STA_NEAR_PRIM_INFEAS_CER:  
    sdp_ret = SDP_RET_NEAR_PRIMAL_INFEASIBLE;
    break;
  case MSK_SOL_STA_NEAR_DUAL_INFEAS_CER:
    sdp_ret = SDP_RET_NEAR_DUAL_INFEASIBLE;
    break;
  case MSK_SOL_STA_UNKNOWN:
  default:
    sdp_ret = SDP_RET_UNKNOWN;
    break;
  }

  if (r != MSK_RES_OK) {
    /* In case of an error print error code and description. */
    printf("An error occurred while optimizing.\n");
    MSK_getcodedesc (r, symname, desc);
    printf("Error %s - '%s'\n", symname, desc);
  }

  MSK_deletetask(&task);
  MSK_deleteenv(&env);

  free(dimvar);
  
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
