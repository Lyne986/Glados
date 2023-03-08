/*
** EPITECH PROJECT, 2023
** B-FUN-500-MPL-5-2-glados-bastien.boymond
** File description:
** files_print
*/

#ifndef FILES_PRINT_H_
#define FILES_PRINT_H_

#include <stdio.h>
#include <string.h>
#include <string.h>
#include <libgen.h>
#include <unistd.h>
#include <stdlib.h>

void write_str_output(char *output, char *path);
void write_int_output(int output, char *path);
void create_empty_file(char *path);
char *get_test_file_path(char *full_path);

#endif /* !FILES_PRINT_H_ */
