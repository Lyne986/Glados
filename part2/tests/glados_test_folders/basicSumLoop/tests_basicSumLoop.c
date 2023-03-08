/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicSumLoop/tests_basicSumLoop.c
*/

#include "files_print.h"

extern int sum(int n);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    write_int_output(sum(10), path);
    write_int_output(sum(0), path);
    return (0);
}
