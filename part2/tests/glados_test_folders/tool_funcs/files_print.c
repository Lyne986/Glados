/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** files_print
*/

#include "files_print.h"

void write_str_output(char *output, char *path)
{
    FILE *file = fopen(path, "a");

    if (file == NULL)
        return;
    fprintf(file, "%s\n", output);
    fclose(file);
}

void create_empty_file(char *path)
{
    FILE *file = fopen(path, "w");

    if (file == NULL)
        return;
    fclose(file);
}

void write_int_output(int output, char *path)
{
    FILE *file = fopen(path, "a");

    if (file == NULL)
        return;
    fprintf(file, "%d\n", output);
    fclose(file);
}
