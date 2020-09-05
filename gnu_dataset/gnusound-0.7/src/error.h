#ifndef ERROR_H
#define ERROR_H

#include <config.h>

#define ERROR_MAGIC 0xABCD

struct error {
    int magic;
    int code;
    char *message;
};

struct error *
ERROR(void *p);

int
error_thrown(struct error *error);

int
error_get_code(struct error *error);

void
error_cascade(struct error *error,
              struct error *source,
              const char *format,
              ...);

const char *
error_get_message(struct error *error);

void
error_set(struct error *error,
          const char *format,
          ...);

void
error_init(struct error *error);

void
error_free(struct error *error);

#endif /* ! ERROR_H */
