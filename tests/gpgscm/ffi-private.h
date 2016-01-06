/* FFI interface for TinySCHEME.
 *
 * Copyright (C) 2016 g10 code GmbH
 *
 * This file is part of GnuPG.
 *
 * GnuPG is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuPG is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _GPGSCM_FFI_PRIVATE_H
#define _GPGSCM_FFI_PRIVATE_H

#include "scheme.h"
#include "scheme-private.h"

#define SC_FFI_PROLOG()                                         \
  unsigned int __ffi_arg_index __attribute__ ((unused)) = 1;    \
  int err __attribute__ ((unused)) = 0                          \

int ffi_bool_value (scheme *sc, pointer p);

#define CONVERSION_pointer(SC, X) (void *) (SC)->vptr->ivalue (X)
#define CONVERSION_number(SC, X) (SC)->vptr->ivalue (X)
#define CONVERSION_string(SC, X) (SC)->vptr->string_value (X)
#define CONVERSION_list(SC, X)	(X)
#define CONVERSION_bool(SC, X)  ffi_bool_value ((SC), (X))
#define CONVERSION_path(SC, X)	(((SC)->vptr->is_string (X)       \
                                  ? (SC)->vptr->string_value	  \
                                  : (SC)->vptr->symname) (X))

#define IS_A_pointer(SC, X)	(SC)->vptr->is_number (X)
#define IS_A_number(SC, X)	(SC)->vptr->is_number (X)
#define IS_A_string(SC, X)	(SC)->vptr->is_string (X)
#define IS_A_list(SC, X)	(SC)->vptr->is_list ((SC), X)
#define IS_A_bool(SC, X)	((X) == (SC)->F || (X) == (SC)->T)
#define IS_A_path(SC, X)	((SC)->vptr->is_string (X)	\
				 || (SC)->vptr->is_symbol (X))

#define SC_ARG(SC, CTYPE, TARGET, WANT, ARGS)                           \
  ({                                                                    \
  if ((ARGS) == (SC)->NIL)						\
    return (SC)->vptr->mk_string ((SC),                                 \
                                  "too few arguments: want "            \
                                  #TARGET "("#WANT"/"#CTYPE")\n");      \
  if (! IS_A_##WANT ((SC), pair_car (ARGS))) {				\
    char __ffi_error_message[256];                                      \
    snprintf (__ffi_error_message, sizeof __ffi_error_message,          \
              "argument %d must be: " #WANT "\n", __ffi_arg_index);     \
    return  (SC)->vptr->mk_string ((SC), __ffi_error_message);          \
  }									\
  TARGET = CONVERSION_##WANT (SC, pair_car (ARGS));			\
  ARGS = pair_cdr (ARGS);						\
  __ffi_arg_index += 1;							\
  })

#define SC_ARGS_DONE(SC, ARGS)                                          \
  ({                                                                    \
  if ((ARGS) != (SC)->NIL)                                              \
    return (SC)->vptr->mk_string ((SC), "too many arguments");          \
  })

#define SC_RETURN_ERR(SC, ERR)					\
  return _cons ((SC), mk_integer ((SC), (ERR)), (SC)->NIL, 1)

#define SC_RETURN(SC)	SC_RETURN_ERR (SC, err)

#define SC_RETURN_POINTER(SC, X)					\
  return _cons ((SC), mk_integer ((SC), err),				\
		_cons ((SC), (X), (SC)->NIL, 1), 1)
#define SC_RETURN_INT(SC, X)						\
  SC_RETURN_POINTER ((SC), mk_integer ((SC), (X)))
#define SC_RETURN_STRING(SC, X)						\
  SC_RETURN_POINTER ((SC), mk_string ((SC), (X)))

const char *schemify_name (const char *s, int macro);

void ffi_scheme_eval (scheme *sc, const char *format, ...)
  __attribute__ ((format (printf, 2, 3)));

#define define_function(S, F)                                   \
  define_function_name ((S), schemify_name (#F, 0), F)

#define define_function_name(S, NAME, F)                                \
  ({                                                                    \
    scheme_define ((S),                                                 \
                   (S)->global_env,                                     \
                   mk_symbol ((S), schemify_name ("_" #F, 0)),          \
                   mk_foreign_func ((S), (do_##F)));                    \
    ffi_scheme_eval ((S),                                               \
                     "(define (%s . a) (ffi-apply \"%s\" %s a))",       \
                     (NAME), (NAME), schemify_name ("_" #F, 0));        \
  })

#define define_constant(S, C)						\
  scheme_define ((S),							\
		 (S)->global_env,					\
		 mk_symbol ((S), schemify_name (#C, 1)),		\
		 mk_integer ((S), (C)))

#define define_(S, SYM, EXP)				\
  scheme_define ((S), (S)->global_env, mk_symbol ((S), (SYM)), EXP)

#define define_variable(S, C)						\
  scheme_define ((S),							\
		 (S)->global_env,					\
		 mk_symbol ((S), schemify_name (#C, 0)),		\
		 mk_integer ((S), (C)))

#define define_variable_pointer(S, C, P)                                \
  scheme_define ((S),							\
		 (S)->global_env,					\
		 mk_symbol ((S), schemify_name (#C, 0)),		\
                 (P))

#define define_variable_string(S, C)                            	\
  define_variable_pointer (S, C, (S)->vptr->mk_string (S, C ?: ""))

void ffi_list2argv (scheme *sc, pointer list, char ***argv);
gpg_error_t ffi_list2intv (scheme *sc, pointer list, int **intv, size_t *len);

#endif /* _GPGSCM_FFI_PRIVATE_H */
