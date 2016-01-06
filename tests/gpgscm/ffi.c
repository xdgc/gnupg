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
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#if HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "../../common/exechelp.h"
#include "../../common/sysutils.h"

#include "private.h"
#include "ffi.h"
#include "ffi-private.h"



int
ffi_bool_value (scheme *sc, pointer p)
{
  return ! (p == sc->F);
}



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
do_setenv (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *name;
  char *value;
  int overwrite;
  SC_ARG (sc, char *, name, string, args);
  SC_ARG (sc, char *, value, string, args);
  SC_ARG (sc, int, overwrite, bool, args);
  SC_ARGS_DONE (sc, args);
  SC_RETURN_ERR (sc, gnupg_setenv (name, value, overwrite));
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

/* XXX: use gnupgs variant b/c mode as string */
static pointer
do_open (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  int fd;
  char *pathname;
  int flags;
  mode_t mode = 0;
  SC_ARG (sc, char *, pathname, path, args);
  SC_ARG (sc, int, flags, number, args);
  if (args != sc->NIL)
    SC_ARG (sc, mode_t, mode, number, args);
  SC_ARGS_DONE (sc, args);

  fd = open (pathname, flags, mode);
  if (fd == -1)
    SC_RETURN_ERR (sc, gpg_error_from_syserror ());
  SC_RETURN_INT (sc, fd);
}

static pointer
do_close (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  int fd;
  SC_ARG (sc, int, fd, number, args);
  SC_ARGS_DONE (sc, args);
  SC_RETURN_ERR (sc, close (fd) == 0 ? 0 : gpg_error_from_syserror ());
}

/* XXX avoid mktemp.  */
static pointer
do_mktemp (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *template;
  char buffer[128];
  SC_ARG (sc, char *, template, string, args);
  SC_ARGS_DONE (sc, args);

  if (strlen (template) > sizeof buffer - 1)
    SC_RETURN_ERR (sc, EINVAL);
  strncpy (buffer, template, sizeof buffer);

  SC_RETURN_STRING (sc, mktemp (buffer));
}

static pointer
do_unlink (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *name;
  SC_ARG (sc, char *, name, string, args);
  SC_ARGS_DONE (sc, args);
  if (unlink (name) == -1)
    SC_RETURN_ERR (sc, gpg_error_from_syserror ());
  SC_RETURN (sc);
}

static pointer
do_getcwd (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  pointer result;
  char *cwd;
  SC_ARGS_DONE (sc, args);
  cwd = gnupg_getcwd ();
  if (cwd == NULL)
    SC_RETURN_ERR (sc, gpg_error_from_syserror ());
  result = sc->vptr->mk_string (sc, cwd);
  free (cwd);
  SC_RETURN_POINTER (sc, result);
}

static pointer
do_mkdir (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *name;
  char *mode;
  SC_ARG (sc, char *, name, string, args);
  SC_ARG (sc, char *, mode, string, args);
  SC_ARGS_DONE (sc, args);
  if (gnupg_mkdir (name, mode) == -1)
    SC_RETURN_ERR (sc, gpg_error_from_syserror ());
  SC_RETURN (sc);
}

static pointer
do_rmdir (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  char *name;
  SC_ARG (sc, char *, name, string, args);
  SC_ARGS_DONE (sc, args);
  if (rmdir (name) == -1)
    SC_RETURN_ERR (sc, gpg_error_from_syserror ());
  SC_RETURN (sc);
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
  SC_ARG (sc, unsigned int, flags, number, args);
  SC_ARGS_DONE (sc, args);

  ffi_list2argv (sc, arguments, &argv);
  if (argv == NULL)
    SC_RETURN_ERR (sc, errno);

  if (verbose > 1)
    {
      char **p;
      fprintf (stderr, "Executing:");
      for (p = argv; *p; p++)
        fprintf (stderr, " '%s'", *p);
      fprintf (stderr, "\n");
    }

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
  SC_ARG (sc, int, infd, number, args);
  SC_ARG (sc, int, outfd, number, args);
  SC_ARG (sc, int, errfd, number, args);
  SC_ARGS_DONE (sc, args);

  ffi_list2argv (sc, arguments, &argv);
  if (argv == NULL)
    SC_RETURN_ERR (sc, errno);

  if (verbose > 1)
    {
      char **p;
      fprintf (stderr, "Executing:");
      for (p = argv; *p; p++)
        fprintf (stderr, " '%s'", *p);
      fprintf (stderr, "\n");
    }

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


static pointer
do_wait_processes (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  pointer list_names;
  char **names;
  pointer list_pids;
  size_t count;
  pid_t *pids;
  int hang;

  SC_ARG (sc, pointer, list_names, list, args);
  SC_ARG (sc, pointer, list_pids, list, args);
  SC_ARG (sc, int, hang, number, args);
  SC_ARGS_DONE (sc, args);

  if (sc->vptr->list_length (sc, list_names)
      != sc->vptr->list_length (sc, list_pids))
    return
      sc->vptr->mk_string (sc, "length of first two arguments must match");

  ffi_list2argv (sc, list_names, &names);
  if (names == NULL)
    SC_RETURN_ERR (sc, gpg_error_from_syserror ());

  err = ffi_list2intv (sc, list_pids, (int **) &pids, &count);
  if (err)
    SC_RETURN (sc);

  err = gnupg_wait_processes ((const char **) names, pids, count, hang, NULL);
  SC_RETURN_INT (sc, 0 /* XXX */);
}


static pointer
do_pipe (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  int filedes[2];
  SC_ARGS_DONE (sc, args);
  err = gnupg_create_pipe (filedes);
#define IMC(A, B)                                                       \
  _cons (sc, sc->vptr->mk_integer (sc, (unsigned long) (A)), (B), 1)
  SC_RETURN_POINTER (sc, IMC (filedes[0],
                              IMC (filedes[1], sc->NIL)));
#undef IMC
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
do_es_feof (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  estream_t stream;
  SC_ARG (sc, estream_t, stream, pointer, args);
  SC_ARGS_DONE (sc, args);

  SC_RETURN_POINTER (sc, es_feof (stream) ? sc->T : sc->F);
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

/* Test helper functions.  */
static pointer
do_file_equal (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  pointer result = sc->F;
  char *a_name, *b_name;
  FILE *a_stream = NULL, *b_stream = NULL;
  struct stat a_stat, b_stat;
#define BUFFER_SIZE	1024
  char a_buf[BUFFER_SIZE], b_buf[BUFFER_SIZE];
#undef BUFFER_SIZE
  size_t size, chunk;

  SC_ARG (sc, char *, a_name, string, args);
  SC_ARG (sc, char *, b_name, string, args);
  SC_ARGS_DONE (sc, args);

  a_stream = fopen (a_name, "rb");
  if (a_stream == NULL)
    goto errout;

  b_stream = fopen (b_name, "rb");
  if (b_stream == NULL)
    goto errout;

  if (fstat (fileno (a_stream), &a_stat) < 0)
    goto errout;

  if (fstat (fileno (b_stream), &b_stat) < 0)
    goto errout;

  if (a_stat.st_size != b_stat.st_size)
    goto out;

  for (size = a_stat.st_size; size > 0; size -= chunk)
    {
      chunk = size;
      if (chunk > sizeof a_buf)
        chunk = sizeof a_buf;

      if (fread (a_buf, 1, chunk, a_stream) < chunk)
        goto errout;
      if (fread (b_buf, 1, chunk, b_stream) < chunk)
        goto errout;
      if (memcmp (a_buf, b_buf, chunk) != 0)
        goto out;
    }

  /* They match.  */
  result = sc->T;

 out:
  if (a_stream)
    fclose (a_stream);
  if (b_stream)
    fclose (b_stream);
  SC_RETURN_POINTER (sc, result);
 errout:
  err = gpg_error_from_syserror ();
  goto out;
}

static pointer
do_splice (scheme *sc, pointer args)
{
  SC_FFI_PROLOG ();
  int source;
  int sink;
  char buffer[1024];
  ssize_t bytes_read;
  SC_ARG (sc, int, source, number, args);
  SC_ARG (sc, int, sink, number, args);
  SC_ARGS_DONE (sc, args);
  while (1)
    {
      bytes_read = read (source, buffer, sizeof buffer);
      if (bytes_read == 0)
        break;
      if (bytes_read < 0)
        SC_RETURN_ERR (sc, gpg_error_from_syserror ());
      if (write (sink, buffer, bytes_read) != bytes_read)
        SC_RETURN_ERR (sc, gpg_error_from_syserror ());
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

gpg_error_t
ffi_list2intv (scheme *sc, pointer list, int **intv, size_t *len)
{
  int i;

  *len = sc->vptr->list_length (sc, list);
  *intv = calloc (*len, sizeof *intv);
  if (*intv == NULL)
    return gpg_error_from_syserror ();

  for (i = 0; sc->vptr->is_pair (list); list = sc->vptr->pair_cdr (list))
    {
      if (sc->vptr->is_number (sc->vptr->pair_car (list)))
        (*intv)[i++] = sc->vptr->ivalue (sc->vptr->pair_car (list));
      else
        return GPG_ERR_INV_ARG;
    }

  return 0;
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
ffi_scheme_eval (scheme *sc, const char *format, ...)
{
  va_list listp;
  char *expression;
  int size, written;

  va_start (listp, format);
  size = vsnprintf (NULL, 0, format, listp);
  va_end (listp);

  expression = malloc (size + 1);
  if (expression == NULL)
    return;

  va_start (listp, format);
  written = vsnprintf (expression, size + 1, format, listp);
  va_end (listp);

  assert (size == written);

  sc->vptr->load_string (sc, expression);
  free (expression);
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
  define_constant (sc, O_APPEND);
#ifndef O_BINARY
# define O_BINARY	0
#endif
#ifndef O_TEXT
# define O_TEXT		0
#endif
  define_constant (sc, O_BINARY);
  define_constant (sc, O_TEXT);
  define_constant (sc, STDIN_FILENO);
  define_constant (sc, STDOUT_FILENO);
  define_constant (sc, STDERR_FILENO);

  define_function (sc, sleep);
  define_function (sc, usleep);
  define_function (sc, chdir);
  define_function (sc, strerror);
  define_function (sc, getenv);
  define_function (sc, setenv);
  define_function (sc, exit);
  define_function (sc, open);
  define_function (sc, close);
  define_function (sc, mktemp);
  define_function (sc, unlink);
  define_function (sc, getcwd);
  define_function (sc, mkdir);
  define_function (sc, rmdir);

  /* Process management.  */
  define_function_name (sc, "spawn-process'", spawn_process);
  define_function (sc, spawn_process_fd);
  define_function (sc, wait_process);
  define_function (sc, wait_processes);
  define_function (sc, pipe);

  /* estream functions.  */
  define_function_name (sc, "es-fclose", es_fclose);
  define_function_name (sc, "es-read", es_read);
  define_function_name (sc, "es-feof", es_feof);
  define_function_name (sc, "es-write", es_write);

  /* Test helper functions.  */
  define_function_name (sc, "file=?", file_equal);
  define_function (sc, splice);

  /* User interface.  */
  define_function (sc, enable_readline);
  define_function (sc, prompt);

  /* Configuration.  */
  ffi_scheme_eval (sc, "(define *verbose* %s)", verbose ? "#t" : "#f");
  define_ (sc, "*argv0*", sc->vptr->mk_string (sc, argv0));
  define_ (sc, "*basedir*", sc->vptr->mk_string (sc, basedir));

  define_ (sc, "*stdin*",
           sc->vptr->mk_port_from_file (sc, stdin, port_input));
  define_ (sc, "*stdout*",
           sc->vptr->mk_port_from_file (sc, stdout, port_output));
  define_ (sc, "*stderr*",
           sc->vptr->mk_port_from_file (sc, stderr, port_output));
  ffi_update (sc);
}

void
ffi_update (scheme *sc)
{
  /* XXX it is not clear whether that is even possible to get right.  */
  (void) sc;
}
