/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/basicWhileLoop/tests_basicWhileLoop.c
*/

#include "files_print.h"

extern int basicWhile(int nb);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");


    write_int_output(basicWhile(10), path);
    return (0);
}
