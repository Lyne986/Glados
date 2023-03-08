/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicVarDefAdd/tests_basicVarDefAdd.c
*/

#include "files_print.h"

extern int varDef();

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");
    write_int_output(varDef(), path);
    return (0);
}
