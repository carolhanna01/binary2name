/*
 * Copyright (C) 2002,2003 Pascal Haakmat.
 * Licensed under the GNU GPL.
 * Absolutely no warranty.
 */

#ifndef TESTS_H
#define TESTS_H

#define TESTS_MSGSZ 100

struct test {
    char *name;
    int (*func)(void *arg);
    void *arg;
};

struct benchmark {
    char *name;
    void (*func)(struct benchmark *b);
    int times;
    char message[TESTS_MSGSZ];
    void *arg;
};

void
tests_printf(int level,
             char *fmt,
             ...);

#endif /* TESTS_H */

