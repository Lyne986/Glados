/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/prototype/tests_prototype.c
*/

#include "files_print.h"

extern void printing();

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    printing();
    create_empty_file(path);
    return (0);
}
