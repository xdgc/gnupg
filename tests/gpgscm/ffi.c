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

#include <config.h>

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <gpg-error.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#if HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "../../common/exechelp.h"

#include "ffi.h"
#include "ffi-private.h"

static pointer
do_logand (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  unsigned int v, acc = ~0;
  while (args != sc->NIL)
    {
      SC_ARG (sc, unsigned int, v, number, args);
      acc &= v;
    }
  SC_RETURN_INT (sc, acc);
}

static pointer
do_logior (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  unsigned int v, acc = 0;
  while (args != sc->NIL)
    {
      SC_ARG (sc, unsigned int, v, number, args);
      acc |= v;
    }
  SC_RETURN_INT (sc, acc);
}

static pointer
do_logxor (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  unsigned int v, acc = 0;
  while (args != sc->NIL)
    {
      SC_ARG (sc, unsigned int, v, number, args);
      acc ^= v;
    }
  SC_RETURN_INT (sc, acc);
}

static pointer
do_lognot (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  unsigned int v;
  SC_ARG (sc, unsigned int, v, number, args);
  SC_ARGS_DONE (sc, args);
  SC_RETURN_INT (sc, ~v);
}

/* User interface.  */

int use_libreadline;

/* Read a string, and return a pointer to it.  Returns NULL on EOF. */
char *
rl_gets (const char *prompt)
{
  static char *line = NULL;
  char *p;
  free (line);

#if HAVE_LIBREADLINE
  if (use_libreadline)
    {
      line = readline (prompt);
      if (line && *line)
        add_history (line);
    }
  else
#endif
    {
      size_t max_size = 0xff;
      printf ("%s", prompt);
      fflush (stdout);
      line = malloc (max_size);
      if (line != NULL)
        fgets (line, max_size, stdin);
    }

  /* Strip trailing whitespace.  */
  if (line && strlen (line) > 0)
    for (p = &line[strlen (line) - 1]; isspace (*p); p--)
      *p = 0;

  return line;
}

static pointer
do_enable_readline (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  SC_ARGS_DONE (sc, args);
  use_libreadline = 1;
  SC_RETURN (sc);
}

static pointer
do_prompt (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  const char *prompt;
  const char *line;
  SC_ARG (sc, const char *, prompt, string, args);
  SC_ARGS_DONE (sc, args);
  line = rl_gets (prompt);
  ffi_update (sc);
  if (! line)
    SC_RETURN_POINTER (sc, sc->EOF_OBJ);

  SC_RETURN_STRING (sc, line);
}

static pointer
do_sleep (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  unsigned int seconds;
  SC_ARG (sc, unsigned int, seconds, number, args);
  SC_ARGS_DONE (sc, args);
  sleep (seconds);
  ffi_update (sc);
  SC_RETURN (sc);
}

static pointer
do_usleep (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  useconds_t microseconds;
  SC_ARG (sc, useconds_t, microseconds, number, args);
  SC_ARGS_DONE (sc, args);
  usleep (microseconds);
  ffi_update (sc);
  SC_RETURN (sc);
}

static pointer
do_chdir (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *name;
  SC_ARG (sc, char *, name, path, args);
  SC_ARGS_DONE (sc, args);
  if (chdir (name))
    SC_RETURN_ERR (sc, errno);
  SC_RETURN (sc);
}

static pointer
do_strerror (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  int error;
  SC_ARG (sc, int, error, number, args);
  SC_ARGS_DONE (sc, args);
  SC_RETURN_STRING (sc, gpg_strerror (error));
}

static pointer
do_getenv (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *name;
  SC_ARG (sc, char *, name, string, args);
  SC_ARGS_DONE (sc, args);
  SC_RETURN_STRING (sc, getenv (name) ?: "");
}

static pointer
do_exit (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  int retcode;
  SC_ARG (sc, int, retcode, number, args);
  SC_ARGS_DONE (sc, args);
  exit (retcode);
}

/* Process handling.  */

static pointer
do_spawn_process (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  pointer arguments;
  char **argv;
  unsigned int flags;

  estream_t infp;
  estream_t outfp;
  estream_t errfp;
  pid_t pid;

  SC_ARG (sc, pointer, arguments, list, args);
  ffi_list2argv (sc, arguments, &argv);
  SC_ARG (sc, unsigned int, flags, number, args);
  SC_ARGS_DONE (sc, args);

  err = gnupg_spawn_process (argv[0], (const char **) &argv[1],
                             GPG_ERR_SOURCE_DEFAULT, /* XXX */
                             NULL, /* XXX */
                             flags,
                             &infp, &outfp, &errfp, &pid);
  free (argv);
#define IMC(A, B)                                                       \
  _cons (sc, sc->vptr->mk_integer (sc, (unsigned long) (A)), (B), 1)
  SC_RETURN_POINTER (sc, IMC (infp,
                              IMC (outfp,
                                   IMC (errfp,
                                        IMC (pid, sc->NIL)))));
#undef IMC
}

static pointer
do_spawn_process_fd (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  pointer arguments;
  char **argv;
  int infd, outfd, errfd;

  pid_t pid;

  SC_ARG (sc, pointer, arguments, list, args);
  ffi_list2argv (sc, arguments, &argv);
  SC_ARG (sc, int, infd, number, args);
  SC_ARG (sc, int, outfd, number, args);
  SC_ARG (sc, int, errfd, number, args);
  SC_ARGS_DONE (sc, args);

  err = gnupg_spawn_process_fd (argv[0], (const char **) &argv[1],
                                infd, outfd, errfd,
                                &pid);
  free (argv);
  SC_RETURN_INT (sc, pid);
}

static pointer
do_wait_process (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  const char *name;
  pid_t pid;
  int hang;

  int retcode;

  SC_ARG (sc, const char *, name, string, args);
  SC_ARG (sc, pid_t, pid, number, args);
  SC_ARG (sc, int, hang, number, args);
  SC_ARGS_DONE (sc, args);
  err = gnupg_wait_process (name, pid, hang, &retcode);
  if (err == GPG_ERR_GENERAL)
    err = 0;	/* Let the return code speak for itself.  */

  ffi_update (sc);
  SC_RETURN_INT (sc, retcode);
}

/* estream functions.  */
static pointer
do_es_fclose (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  estream_t stream;
  SC_ARG (sc, estream_t, stream, pointer, args);
  SC_ARGS_DONE (sc, args);
  SC_RETURN_ERR (sc, es_fclose (stream));
}

static pointer
do_es_read (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  estream_t stream;
  size_t bytes_to_read;

  pointer result;
  void *buffer;
  size_t bytes_read;

  SC_ARG (sc, estream_t, stream, pointer, args);
  SC_ARG (sc, size_t, bytes_to_read, number, args);
  SC_ARGS_DONE (sc, args);

  buffer = malloc (bytes_to_read);
  if (buffer == NULL)
    SC_RETURN_ERR (sc, ENOMEM);

  err = es_read (stream, buffer, bytes_to_read, &bytes_read);
  if (err)
    SC_RETURN_ERR (sc, err);

  result = sc->vptr->mk_counted_string (sc, buffer, bytes_read);
  free (buffer);
  SC_RETURN_POINTER (sc, result);
}

static pointer
do_es_write (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  estream_t stream;
  const char *buffer;
  size_t bytes_to_write, bytes_written;

  SC_ARG (sc, estream_t, stream, pointer, args);
  /* XXX how to get the length of the string buffer?  scheme strings
     may contain \0.  */
  SC_ARG (sc, const char *, buffer, string, args);
  SC_ARGS_DONE (sc, args);

  bytes_to_write = strlen (buffer);
  while (bytes_to_write > 0)
    {
      err = es_write (stream, buffer, bytes_to_write, &bytes_written);
      if (err)
        break;
      bytes_to_write -= bytes_written;
      buffer += bytes_written;
    }

  SC_RETURN (sc);
}

void
ffi_list2argv (scheme *sc, pointer list, char ***argv)
{
  int i, length;

  length = sc->vptr->list_length (sc, list);
  *argv = calloc (length + 1, sizeof **argv);
  if (*argv == NULL)
    return;

  for (i = 0; sc->vptr->is_pair (list); list = sc->vptr->pair_cdr (list))
    {
      if (sc->vptr->is_string (sc->vptr->pair_car (list)))
        (*argv)[i++] = sc->vptr->string_value (sc->vptr->pair_car (list));
      else if (sc->vptr->is_symbol (sc->vptr->pair_car (list)))
        (*argv)[i++] = sc->vptr->symname (sc->vptr->pair_car (list));
      else
        continue; /* XXX this just silently drops values */
    }
  (*argv)[i] = NULL;
}

const char *
schemify_name (const char *s, int macro)
{
  char *n = strdup (s), *p;
  if (n == NULL)
    return s;
  for (p = n; *p; p++)
    {
      *p = (char) tolower (*p);
       /* We convert _ to - in identifiers.  We allow, however, for
	  function names to start with a leading _.  The functions in
	  this namespace are not yet finalized and might change or
	  vanish without warning.  Use them with care.	*/
      if (! macro
	  && p != n
	  && *p == '_')
	*p = '-';
    }
  return n;
}

void
ffi_init (scheme *sc)
{
  /* bitwise arithmetic */
  define_function (sc, logand);
  define_function (sc, logior);
  define_function (sc, logxor);
  define_function (sc, lognot);

  /* libc.  */
  define_constant (sc, O_RDONLY);
  define_constant (sc, O_WRONLY);
  define_constant (sc, O_RDWR);
  define_constant (sc, O_CREAT);

  define_function (sc, sleep);
  define_function (sc, usleep);
  define_function (sc, chdir);
  define_function (sc, strerror);
  define_function (sc, getenv);
  define_function (sc, exit);

  /* Process management.  */
  define_function_name (sc, "spawn-process'", spawn_process);
  define_function (sc, spawn_process_fd);
  define_function (sc, wait_process);

  /* estream functions.  */
  define_function_name (sc, "es-fclose", es_fclose);
  define_function_name (sc, "es-read", es_read);
  define_function_name (sc, "es-write", es_write);

  /* User interface.  */
  define_function (sc, enable_readline);
  define_function (sc, prompt);

  ffi_update (sc);
}

void
ffi_update (scheme *sc)
{
  /* Currently, this function does nothing.  */
  (void) sc;
}
