/*
 * OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
 * programming (SDP) solvers.
 * Copyright (C) 2012, 2014, 2015  P. Roux and P.L. Garoche
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* This little tool takes as input a matrix of rationals of size nxn and returns
 * 1 if it manages to prove that the matrix is symmetric positive definite or 0
 * otherwise (i.e. the matrix is either not symmetric positive definite or its
 * smallest eigenvalue is too small for the proof to succeed).
 *
 * The proof is based on a Cholesky decomposition hence done in time O(n^3).
 * See the following paper for details of the proof:
 *   S.M. Rump: Verification of positive definiteness, 
 *   BIT Numerical Mathematics, 46:433-452, 2006. */

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <stdlib.h>
#include <fenv.h>
#include <math.h>

typedef struct {
  int s;
  double *t;
} matrix_t;

static void *malloc_fail(int nb, int size)
{
  void *p;

  p = malloc(nb * size);
  if (p == NULL) caml_failwith("caml_osdp: out of memory!");

  return p;
}

static matrix_t *matrix_new(int n)
{
  matrix_t *p = malloc_fail(1, sizeof(matrix_t));

  p->s = n;
  p->t = malloc_fail(n * n, sizeof(double));

  return p;
}

static void matrix_free(matrix_t *p)
{
  free(p->t);
  free(p);
}

#define SEL(m, i, j) ((m)->t[(i)*((m)->s)+(j)])

static void rnd(int mode)
{
  if (fesetround(mode))
    caml_failwith("caml_osdp: unable to change rounding mode!");
}

/* parse matrix, returning two matrices of doubles mp and radp such
 * that for all i, j if (a, b) = mll_i,j then [a, b] \subseteq { x |
 * |x - mp_i,j| <= radp_i,j }. Sets rounding mode to FE_UPWARD. */
static void parse_matrix(value mll, matrix_t **mp, matrix_t **radp)
{
  matrix_t *m, *rad;
  int i, j, sz;
  value line;
  double d1, d2;
  
  sz = Wosize_val(mll);
  *mp = m = matrix_new(sz);
  *radp = rad = matrix_new(sz);

  for (i = 0; i < sz; ++i) {
    line = Field(mll, i);
    for (j = 0; j < sz; ++j)
      SEL(m, i, j) = (Double_val(Field(Field(line, j), 0))
                      + Double_val(Field(Field(line, j), 1))) / 2.;
  }

  rnd(FE_UPWARD);
  for (i = 0; i < m->s; ++i) {
    line = Field(mll, i);
    for (j = 0; j < m->s; ++j) {
      d1 = Double_val(Field(Field(line, j), 1)) - SEL(m, i, j);
      d2 = SEL(m, i, j) - Double_val(Field(Field(line, j), 1));
      SEL(rad, i, j) = d2 > d1 ? d2 : d1;
    }
  }
}

/* Returns 1 if m is symmetric and finite, 0 otherwise. */
static int is_symmetric_finite(matrix_t *m)
{
  int i, j;

  for (i = 0; i < m->s; ++i)
    for (j = 0; j < i; ++j)
      if (SEL(m, i, j) != SEL(m, j, i) || !isfinite(SEL(m, i, j))) return 0;
      
  return 1;
}

/* Try Cholesky decomposition on symmetric matrix m.
 * Returns 1 if runs to completion, 0 otherwise. */
static int chol(matrix_t *m)
{
  int i, j, k;
  double s;
  matrix_t *r = matrix_new(m->s);

  for (j = 0; j < m->s; ++j) {
    for (i = 0; i < j; ++i) {
      s = SEL(m, i, j);
      for (k = 0; k < i; ++k)
        s -= SEL(r, k, i) * SEL(r, k, j);
      SEL(r, i, j) = s / SEL(r, i, i);
    }
    s = SEL(m, j, j);
    for (k = 0; k < j; ++k)
      s -= SEL(r, k, j) * SEL(r, k, j);
    if (s <= 0) {
      matrix_free(r);
      return 0;
    }
    SEL(r, j, j) = sqrt(s);
  }
  
  matrix_free(r);
  return 1;
}

static int check(value mll)
{
  double eps = 0x1p-53, eta = 0x1p-1074;
  int old_rnd;
  double gamma_np1, c;
  double tr, maxdiag, maxrad;
  int i, j;
  matrix_t *m, *rad;

  old_rnd = fegetround();
  
  parse_matrix(mll, &m, &rad);
  /* round mode is now FE_UPWARD */
  
  /* the test only works for matrices of size < 2^51 - 2 */
  if (m->s > 1000000000) goto out_no;

  /* symmmetry check */
  if (!is_symmetric_finite(m) || !is_symmetric_finite(rad)) goto out_no;

  /* diagonal check */
  for (i = 0; i < m->s; ++i)
    if (SEL(m, i, i) <= 0) goto out_no;

  if (m->s == 1) goto out_yes;

  gamma_np1 = m->s * eps - 1;
  gamma_np1 = -gamma_np1;  /* 1-n*eps rounded downward */
  gamma_np1 = m->s * eps / gamma_np1;  /* \gamma_{n+1} = n*eps/(1-n*eps) */

  c = gamma_np1 - 1;
  c = -c;  /* 1-gamma_np1 rounded downward */
  c = gamma_np1 / c;  /* gamma_np1 / (1-gamma_np1) */

  tr = 0;
  maxdiag = 0;
  for (i = 0; i < m->s; ++i) {
    tr += SEL(m, i, i);
    if (SEL(m, i, i) > maxdiag)
      maxdiag = SEL(m, i, i);
  }  /* tr = tr(m) */

  /* 2 * eta is actually 4 * 0x1p-1075 (but 0x1p-1075 would underflow to 0) */
  c = c * tr + 2 * eta * (m->s + 1) * (2 * (m->s + 2) + maxdiag);

  maxrad = 0;
  for (i = 0; i < m->s; ++i) {
    for (j = 0; j < m->s; ++j) {
      if (SEL(rad, i, j) > maxrad) maxrad = SEL(rad, i, j);
    }
  }

  c += maxrad * (m->s + 1);

  rnd(FE_DOWNWARD);
  for (i = 0; i < m->s; ++i) {
    SEL(m, i, i) -= c;
    if (!isfinite(SEL(m, i, i))) goto out_no;
  }
    
  rnd(FE_TONEAREST);
  if (chol(m)) goto out_yes;
  else goto out_no;

 out_no:
  fesetround(old_rnd);
  matrix_free(m);
  matrix_free(rad);
  return 0;
 out_yes:
  fesetround(old_rnd);
  matrix_free(m);
  matrix_free(rad);
  return 1;
}

value posdef_check(value mll)
{
  CAMLparam1(mll);

  int res;

  res = check(mll);

  CAMLreturn(Val_int(res));
}
