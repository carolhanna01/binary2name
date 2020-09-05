#ifndef GGO_OPTIONS_H
#define GGO_OPTIONS_H

extern "C"
{
#include "ggos.h"
};

extern struct gengetopt_option * gengetopt_options;

#define foropt for (opt = gengetopt_options;             \
                    opt != (struct gengetopt_option *)0; \
                    opt=opt->next)

#endif /* GGO_OPTIONS_H */
