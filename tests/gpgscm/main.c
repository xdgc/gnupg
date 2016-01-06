/* TinyScheme-based test driver.
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
#include <gcrypt.h>
#include <gpg-error.h>
#include <npth.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "scheme.h"
#include "ffi.h"
#include "i18n.h"
#include "../../common/argparse.h"
#include "../../common/init.h"
#include "../../common/logging.h"
#include "../../common/strlist.h"

/* The TinyScheme banner.  Unfortunately, it isn't in the header
   file.  */
#define ts_banner "TinyScheme 1.41"

int verbose;



/* Constants to identify the commands and options. */
enum cmd_and_opt_values
  {
    aNull = 0,
    oBase	= 500,
  };

/* The list of commands and options. */
static ARGPARSE_OPTS opts[] =
  {
    ARGPARSE_s_s (oBase,   "base", "base directory (containing 'init.scm')"),
    ARGPARSE_end (),
  };

const char *basedir = "../tests/gpgscm";

/* Command line parsing.  */
static void
parse_arguments (ARGPARSE_ARGS *pargs, ARGPARSE_OPTS *popts)
{
  int no_more_options = 0;

  while (!no_more_options && optfile_parse (NULL, NULL, NULL, pargs, popts))
    {
      switch (pargs->r_opt)
        {
        case oBase:
	  basedir = pargs->r.ret_str;
	  break;

        default:
	  pargs->err = 2;
	  break;
	}
    }
}

/* Print usage information and and provide strings for help. */
static const char *
my_strusage( int level )
{
  const char *p;

  switch (level)
    {
    case 11: p = "gpgscm (@GNUPG@)";
      break;
    case 13: p = VERSION; break;
    case 17: p = PRINTABLE_OS_NAME; break;
    case 19: p = _("Please report bugs to <@EMAIL@>.\n"); break;

    case 1:
    case 40:
      p = _("Usage: gpgscm [options] [file] (-h for help)");
      break;
    case 41:
      p = _("Syntax: gpgscm [options] [file]\n"
            "Execute the given Scheme program, or spawn interactive shell.\n");
      break;

    default: p = NULL; break;
    }
  return p;
}


/* Load the Scheme program from FILE_NAME.  If FILE_NAME is not an
   absolute path, and PATH is not NULL, then it is qualified with
   PATH.  */
void
load (scheme *sc, const char *path, const char *file_name)
{
  char *qualified_name;
  FILE *h;

  if (path != NULL && file_name[0] != '/')
    {
      if (asprintf (&qualified_name, "%s/%s", path, file_name) < 0)
        {
          fprintf (stderr, "asprintf: %s\n", strerror (errno));
          exit (EXIT_FAILURE);
        }
    }
  else
    qualified_name = (char *) file_name;

  if (verbose)
    fprintf (stderr, "Loading %s...\n", qualified_name);
  h = fopen (qualified_name, "r");
  if (! h)
    {
      fprintf (stderr, "Could not read %s: %s.\n"
               "Consider using --base to specify the location of the "
               "Scheme library.\n", qualified_name, strerror (errno));
      exit (EXIT_FAILURE);
    }

  scheme_load_named_file (sc, h, qualified_name);
  fclose (h);

  if (file_name != qualified_name)
    free (qualified_name);
}



int
main (int argc, char **argv)
{
  ARGPARSE_ARGS pargs;
  scheme *sc;

  if (getenv ("GPGSCM_BASE"))
    basedir = getenv ("GPGSCM_BASE");

  set_strusage (my_strusage);
  log_set_prefix ("gpgscm", 1);

  /* Make sure that our subsystems are ready.  */
  i18n_init ();
  init_common_subsystems (&argc, &argv);

  if (!gcry_check_version (GCRYPT_VERSION))
    {
      fputs ("libgcrypt version mismatch\n", stderr);
      exit (2);
    }

  /* Parse the command line. */
  pargs.argc  = &argc;
  pargs.argv  = &argv;
  pargs.flags = 0;
  parse_arguments (&pargs, opts);

  if (log_get_errorcount (0))
    exit (2);

  sc = scheme_init_new ();
  if (! sc) {
    fprintf (stderr, "Could not initialize TinyScheme!\n");
    return 2;
  }
  scheme_set_input_port_file (sc, stdin);
  scheme_set_output_port_file (sc, stdout);

  load (sc, basedir, "init.scm");
  load (sc, basedir, "ffi.scm");
  load (sc, basedir, "lib.scm");
  load (sc, basedir, "tests.scm");

  ffi_init (sc);

  if (argc == 0)
    {
      /* Interactive shell.  */
      fprintf (stderr, "gpgscm/"ts_banner".\n");
      scheme_load_named_file (sc, stdin, 0);
    }
  else
    load (sc, NULL, argv[0]);

  scheme_deinit (sc);
  return EXIT_SUCCESS;
}
