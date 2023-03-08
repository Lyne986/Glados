/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/longsum/tests_longsum.c
*/

#include "files_print.h"

extern long sum(long, long, long, long);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    write_int_output(sum(1, 2, 3, 4), path);
    return (0);
}
