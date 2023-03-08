/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/varRedef/tests_varRedef.c
*/

#include "files_print.h"

extern int redef();

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(redef(), path);
    return (0);
}
