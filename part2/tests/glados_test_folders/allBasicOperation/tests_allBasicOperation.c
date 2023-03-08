/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** glados_test_folders/allBasicOperation/tests_allBasicOperation.c
*/

#include "files_print.h"

extern int my_add(int a, int b);
extern int my_mul(int a, int b);
extern int my_mod(int a, int b);
extern int my_div(int a, int b);

int main(int argc, char **argv)
{
    char *path = strcat(get_test_file_path(__FILE__), "/output");

    write_int_output(my_add(10, 20), path);
    write_int_output(my_add(0, 0), path);
    write_int_output(my_add(-10, 20), path);
    write_int_output(my_add(10, -20), path);
    write_int_output(my_add(-10, -20), path);
    write_int_output(my_mul(10, 20), path);
    write_int_output(my_mul(0, 0), path);
    write_int_output(my_mul(-10, 20), path);
    write_int_output(my_mul(10, -20), path);
    write_int_output(my_mul(-10, -20), path);
    write_int_output(my_mod(10, 2), path);
    write_int_output(my_mod(10, 3), path);
    write_int_output(my_div(10, 2), path);
    return (0);
}
