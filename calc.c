// Zahra Guluzade 251adc008
// https://github.com/glzdzahra/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: Gemini AI (Google) for recursive descent parser structure, 1-based error position tracking, and command-line directory handling.

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>

// Structure to store number values (can be integer or float)
typedef struct {
    int is_float;
    long long i_val;
    double f_val;
} Number;

// Token types for our parser
typedef enum {
    TOK_NUM, TOK_PLUS, TOK_MINUS, TOK_MUL, TOK_DIV, 
    TOK_POW, TOK_LPAREN, TOK_RPAREN, TOK_NEWLINE, TOK_EOF, TOK_ERR
} TokenType;

typedef struct {
    TokenType type;
    int is_float;
    long long i_val;
    double f_val;
    long pos; // 1-based position in file
} Token;

// Global parser state variables
static const char *input_src = NULL;
static long input_len = 0;
static long current_pos = 0;
static Token current_token;
static int error_occurred = 0;
static long error_position = 0;

// Forward declarations for recursive descent functions
static void get_next_token(void);
static Number parse_expression(void);

// Set error and remember the position of the first error encountered
static void trigger_error(long pos) {
    if (!error_occurred) {
        error_occurred = 1;
        error_position = pos;
    }
}

// Tokenizer: reads characters from source and builds tokens with 1-based positions
static void get_next_token(void) {
    if (error_occurred) return;

    while (current_pos < input_len) {
        char c = input_src[current_pos];

        // Skip spaces and tabs
        if (c == ' ' || c == '\t' || c == '\r') {
            current_pos++;
            continue;
        }

        // Handle newlines
        if (c == '\n') {
            current_token.type = TOK_NEWLINE;
            current_token.pos = current_pos + 1;
            current_pos++;
            return;
        }

        // Handle comment lines starting with #
        if (c == '#') {
            long check = current_pos - 1;
            int first_on_line = 1;
            while (check >= 0 && input_src[check] != '\n') {
                if (input_src[check] != ' ' && input_src[check] != '\t') {
                    first_on_line = 0;
                    break;
                }
                check--;
            }
            if (!first_on_line) {
                trigger_error(current_pos + 1);
                current_token.type = TOK_ERR;
                return;
            }
            while (current_pos < input_len && input_src[current_pos] != '\n') {
                current_pos++;
            }
            continue;
        }

        long start_pos = current_pos + 1;

        if (c == '+') { current_token.type = TOK_PLUS; current_token.pos = start_pos; current_pos++; return; }
        if (c == '-') { current_token.type = TOK_MINUS; current_token.pos = start_pos; current_pos++; return; }
        if (c == '/') { current_token.type = TOK_DIV; current_token.pos = start_pos; current_pos++; return; }
        if (c == '(') { current_token.type = TOK_LPAREN; current_token.pos = start_pos; current_pos++; return; }
        if (c == ')') { current_token.type = TOK_RPAREN; current_token.pos = start_pos; current_pos++; return; }
        
        if (c == '*') {
            if (current_pos + 1 < input_len && input_src[current_pos + 1] == '*') {
                current_token.type = TOK_POW;
                current_token.pos = start_pos;
                current_pos += 2;
            } else {
                current_token.type = TOK_MUL;
                current_token.pos = start_pos;
                current_pos++;
            }
            return;
        }

        // Parse numbers (integers or floats)
        if (isdigit((unsigned char)c) || c == '.') {
            long num_start = current_pos;
            int is_flt = 0;
            int has_before = 0, has_after = 0;

            while (current_pos < input_len && isdigit((unsigned char)input_src[current_pos])) {
                has_before = 1;
                current_pos++;
            }

            if (current_pos < input_len && input_src[current_pos] == '.') {
                is_flt = 1;
                current_pos++;
                while (current_pos < input_len && isdigit((unsigned char)input_src[current_pos])) {
                    has_after = 1;
                    current_pos++;
                }
            }

            if (current_pos < input_len && (input_src[current_pos] == 'e' || input_src[current_pos] == 'E')) {
                is_flt = 1;
                long e_backup = current_pos;
                current_pos++;
                if (current_pos < input_len && (input_src[current_pos] == '+' || input_src[current_pos] == '-')) {
                    current_pos++;
                }
                int has_e_digits = 0;
                while (current_pos < input_len && isdigit((unsigned char)input_src[current_pos])) {
                    has_e_digits = 1;
                    current_pos++;
                }
                if (!has_e_digits) current_pos = e_backup;
            }

            if (is_flt && (!has_before || !has_after)) {
                trigger_error(num_start + 1);
                current_token.type = TOK_ERR;
                return;
            }

            char temp_buf[64];
            long len_to_copy = current_pos - num_start;
            if (len_to_copy >= (long)sizeof(temp_buf)) len_to_copy = sizeof(temp_buf) - 1;
            strncpy(temp_buf, input_src + num_start, len_to_copy);
            temp_buf[len_to_copy] = '\0';

            current_token.pos = start_pos;
            if (is_flt) {
                current_token.type = TOK_NUM;
                current_token.is_float = 1;
                current_token.f_val = atof(temp_buf);
            } else {
                current_token.type = TOK_NUM;
                current_token.is_float = 0;
                current_token.i_val = atoll(temp_buf);
            }
            return;
        }

        trigger_error(start_pos);
        current_token.type = TOK_ERR;
        return;
    }

    current_token.type = TOK_EOF;
    current_token.pos = input_len + 1;
}

// Primary parser: numbers and parentheses
static Number parse_primary(void) {
    Number num = {0, 0, 0.0};
    if (error_occurred) return num;

    if (current_token.type == TOK_NUM) {
        num.is_float = current_token.is_float;
        num.i_val = current_token.i_val;
        num.f_val = current_token.f_val;
        get_next_token();
        return num;
    }

    if (current_token.type == TOK_LPAREN) {
        get_next_token();
        num = parse_expression();
        if (error_occurred) return num;

        if (current_token.type == TOK_RPAREN) {
            get_next_token();
            return num;
        } else {
            trigger_error(current_token.pos);
            return num;
        }
    }

    trigger_error(current_token.pos);
    return num;
}

// Power parser: handles ** operator with right-associativity
static Number parse_power(void) {
    Number left = parse_primary();
    if (error_occurred) return left;

    if (current_token.type == TOK_POW) {
        long op_pos = current_token.pos;
        get_next_token();
        Number right = parse_power();
        if (error_occurred) return left;

        Number res;
        if (left.is_float || right.is_float) {
            double l = left.is_float ? left.f_val : (double)left.i_val;
            double r = right.is_float ? right.f_val : (double)right.i_val;

            if (l < 0.0 && r != floor(r)) {
                trigger_error(op_pos);
                return left;
            }
            double val = pow(l, r);
            if (isnan(val) || isinf(val)) {
                trigger_error(op_pos);
                return left;
            }
            res.is_float = 1;
            res.f_val = val;
        } else {
            if (right.i_val < 0) {
                trigger_error(op_pos);
                return left;
            }
            long long base = left.i_val;
            long long exp = right.i_val;
            long long pow_res = 1;
            while (exp > 0) {
                if (exp % 2 == 1) pow_res *= base;
                base *= base;
                exp /= 2;
            }
            res.is_float = 0;
            res.i_val = pow_res;
        }
        return res;
    }
    return left;
}

// Term parser: handles multiplication and division
static Number parse_term(void) {
    Number left = parse_power();
    if (error_occurred) return left;

    while (current_token.type == TOK_MUL || current_token.type == TOK_DIV) {
        TokenType op = current_token.type;
        long op_pos = current_token.pos;
        get_next_token();
        Number right = parse_power();
        if (error_occurred) return left;

        if (op == TOK_MUL) {
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f_val : (double)left.i_val;
                double r = right.is_float ? right.f_val : (double)right.i_val;
                left.is_float = 1;
                left.f_val = l * r;
            } else {
                left.i_val *= right.i_val;
            }
        } else {
            if (!right.is_float && right.i_val == 0) {
                trigger_error(op_pos);
                return left;
            }
            if (right.is_float && right.f_val == 0.0) {
                trigger_error(op_pos);
                return left;
            }
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f_val : (double)left.i_val;
                double r = right.is_float ? right.f_val : (double)right.i_val;
                left.is_float = 1;
                left.f_val = l / r;
            } else {
                left.i_val /= right.i_val;
            }
        }
    }
    return left;
}

// Expression parser: handles addition and subtraction
static Number parse_expression(void) {
    Number left = parse_term();
    if (error_occurred) return left;

    while (current_token.type == TOK_PLUS || current_token.type == TOK_MINUS) {
        TokenType op = current_token.type;
        get_next_token();
        Number right = parse_term();
        if (error_occurred) return left;

        if (op == TOK_PLUS) {
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f_val : (double)left.i_val;
                double r = right.is_float ? right.f_val : (double)right.i_val;
                left.is_float = 1;
                left.f_val = l + r;
            } else {
                left.i_val += right.i_val;
            }
        } else {
            if (left.is_float || right.is_float) {
                double l = left.is_float ? left.f_val : (double)left.i_val;
                double r = right.is_float ? right.f_val : (double)right.i_val;
                left.is_float = 1;
                left.f_val = l - r;
            } else {
                left.i_val -= right.i_val;
            }
        }
    }
    return left;
}

// Process a single input file
static void process_single_file(const char *in_file, const char *out_file) {
    FILE *fin = fopen(in_file, "rb");
    if (!fin) {
        fprintf(stderr, "Error opening input file: %s\n", in_file);
        exit(1);
    }

    fseek(fin, 0, SEEK_END);
    long len = ftell(fin);
    fseek(fin, 0, SEEK_SET);

    char *buf = (char *)malloc(len + 1);
    if (!buf) {
        fclose(fin);
        fprintf(stderr, "Memory allocation error\n");
        exit(1);
    }

    if (len > 0 && fread(buf, 1, len, fin) != (size_t)len) {
        free(buf);
        fclose(fin);
        fprintf(stderr, "File read error\n");
        exit(1);
    }
    buf[len] = '\0';
    fclose(fin);

    input_src = buf;
    input_len = len;
    current_pos = 0;
    error_occurred = 0;
    error_position = 0;

    get_next_token();

    char *out_results = (char *)malloc(len * 32 + 1024);
    out_results[0] = '\0';
    long out_idx = 0;

    while (current_token.type != TOK_EOF && !error_occurred) {
        if (current_token.type == TOK_NEWLINE) {
            get_next_token();
            continue;
        }

        Number res = parse_expression();
        if (error_occurred) break;

        if (current_token.type == TOK_NEWLINE || current_token.type == TOK_EOF) {
            if (res.is_float) {
                out_idx += sprintf(out_results + out_idx, "%.15g\n", res.f_val);
            } else {
                out_idx += sprintf(out_results + out_idx, "%lld\n", res.i_val);
            }
            if (current_token.type == TOK_NEWLINE) get_next_token();
        } else {
            trigger_error(current_token.pos);
            break;
        }
    }

    FILE *fout = fopen(out_file, "wb");
    if (!fout) {
        free(buf);
        free(out_results);
        fprintf(stderr, "Error creating output file: %s\n", out_file);
        exit(1);
    }

    if (error_occurred) {
        fprintf(fout, "ERROR:%ld\n", error_position);
    } else {
        fputs(out_results, fout);
    }

    fclose(fout);
    free(buf);
    free(out_results);
}

// Helper to create directories recursively
static void create_directories(const char *path) {
    char tmp[2048];
    snprintf(tmp, sizeof(tmp), "%s", path);
    size_t len = strlen(tmp);
    if (len > 0 && tmp[len - 1] == '/') tmp[len - 1] = '\0';

    for (char *p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = '\0';
            mkdir(tmp, 0755);
            *p = '/';
        }
    }
    mkdir(tmp, 0755);
}

// Extract base filename without path and extension
static void get_filename_base(const char *path, char *out, size_t max_size) {
    const char *slash = strrchr(path, '/');
    const char *start = slash ? slash + 1 : path;
    strncpy(out, start, max_size - 1);
    out[max_size - 1] = '\0';
    char *dot = strrchr(out, '.');
    if (dot && strcmp(dot, ".txt") == 0) {
        *dot = '\0';
    }
}

int main(int argc, char *argv[]) {
    const char *dir_flag = NULL;
    const char *out_flag = NULL;
    const char *input_file = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 < argc) dir_flag = argv[++i];
            else { fprintf(stderr, "Missing directory argument\n"); return 1; }
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 < argc) out_flag = argv[++i];
            else { fprintf(stderr, "Missing output directory argument\n"); return 1; }
        } else {
            input_file = argv[i];
        }
    }

    const char *surname = "guluzade";
    const char *studentid = "251adc008";

    if (dir_flag != NULL) {
        DIR *d = opendir(dir_flag);
        if (!d) {
            fprintf(stderr, "Cannot open directory: %s\n", dir_flag);
            return 1;
        }

        char out_dir[2048];
        if (out_flag) {
            snprintf(out_dir, sizeof(out_dir), "%s", out_flag);
        } else {
            snprintf(out_dir, sizeof(out_dir), "%s_%s_%s", dir_flag, surname, studentid);
        }
        create_directories(out_dir);

        struct dirent *entry;
        while ((entry = readdir(d)) != NULL) {
            const char *ext = strrchr(entry->d_name, '.');
            if (ext && strcmp(ext, ".txt") == 0) {
                char in_path[4096], out_path[4096], base[256];
                snprintf(in_path, sizeof(in_path), "%s/%s", dir_flag, entry->d_name);
                get_filename_base(entry->d_name, base, sizeof(base));
                snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s.txt", out_dir, base, surname, studentid);
                process_single_file(in_path, out_path);
            }
        }
        closedir(d);
        return 0;
    }

    if (input_file != NULL) {
        char base[256];
        get_filename_base(input_file, base, sizeof(base));

        char out_dir[4096];
        if (out_flag) {
            snprintf(out_dir, sizeof(out_dir), "%s", out_flag);
        } else {
            snprintf(out_dir, sizeof(out_dir), "%s_%s_%s", base, surname, studentid);
        }
        create_directories(out_dir);

        char out_path[4096];
        snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s.txt", out_dir, base, surname, studentid);
        process_single_file(input_file, out_path);
        return 0;
    }

    fprintf(stderr, "Error: No input file or directory provided.\n");
    return 1;
}