#include "s21_matrix.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int checkingMatrix(matrix_t *A) {
  int answer = 1;
  if (A == NULL || A->matrix == NULL || A->columns <= 0 || A->rows <= 0) {
    answer = 0;
  }
  return answer;
}

int s21_create_matrix(int rows, int columns, matrix_t *result) {
  int ret = INCORRECT;
  if (rows > 0 && columns > 0) {
    double **matrix =
        calloc(rows * columns * sizeof(double) + rows * sizeof(double *), 1);
    if (matrix) {
      double *ptr = (double *)(matrix + rows);
      for (int i = 0; i < rows; i++) {
        matrix[i] = ptr + columns * i;
      }
      result->columns = columns;
      result->matrix = matrix;
      result->rows = rows;
      ret = OK;
    }
  }
  return ret;
}

void s21_remove_matrix(matrix_t *A) {
  if (A->matrix) {
    free(A->matrix);
  }
  A->matrix = NULL;
  A->columns = 0;
  A->rows = 0;
}

int s21_eq_matrix(matrix_t *A, matrix_t *B) {
  int answer = SUCCESS;
  if (checkingMatrix(A) == 1 && checkingMatrix(B) == 1 && A->rows == B->rows &&
      A->columns == B->columns) {
    for (int i = 0; i < A->rows; i++) {
      for (int j = 0; j < A->columns; j++) {
        if (fabs(A->matrix[i][j] - B->matrix[i][j]) > 1e-7) {
          answer = FAILURE;
          break;
        }
      }
      if (answer == FAILURE) {
        break;
      }
    }
  } else {
    answer = FAILURE;
  }
  return answer;
}

int s21_sum_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1 && checkingMatrix(B) == 1) {
    if (A->rows == B->rows && A->columns == B->columns) {
      answer = s21_create_matrix(A->rows, A->columns, result);
      if (answer == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < A->columns; j++) {
            result->matrix[i][j] = A->matrix[i][j] + B->matrix[i][j];
          }
        }
      } else {
        answer = INCORRECT;
      }
    } else {
      answer = CALC_ERROR;
    }
  } else {
    answer = INCORRECT;
  }

  return answer;
}

int s21_sub_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1 && checkingMatrix(B) == 1) {
    if (A->rows == B->rows && A->columns == B->columns) {
      answer = s21_create_matrix(A->rows, A->columns, result);
      if (answer == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < A->columns; j++) {
            result->matrix[i][j] = A->matrix[i][j] - B->matrix[i][j];
          }
        }
      } else {
        answer = INCORRECT;
      }
    } else {
      answer = CALC_ERROR;
    }
  } else {
    answer = INCORRECT;
  }

  return answer;
}

int s21_mult_number(matrix_t *A, double number, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1) {
    answer = s21_create_matrix(A->rows, A->columns, result);
    if (answer == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++) {
          result->matrix[i][j] = A->matrix[i][j] * number;
        }
      }
    } else {
      answer = INCORRECT;
    }
  } else {
    answer = INCORRECT;
  }
  return answer;
}

int s21_mult_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1 && checkingMatrix(B) == 1) {
    if (A->columns == B->rows) {
      answer = s21_create_matrix(A->rows, B->columns, result);
      if (answer == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < B->columns; j++) {
            for (int k = 0; k < B->rows; k++) {
              result->matrix[i][j] += A->matrix[i][k] * B->matrix[k][j];
            }
          }
        }
      } else {
        answer = INCORRECT;
      }
    } else {
      answer = CALC_ERROR;
    }
  } else {
    answer = INCORRECT;
  }
  return answer;
}

int s21_transpose(matrix_t *A, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1) {
    answer = s21_create_matrix(A->columns, A->rows, result);
    if (answer == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++) {
          result->matrix[j][i] = A->matrix[i][j];
        }
      }
    }
  } else {
    answer = INCORRECT;
  }
  return answer;
}

int s21_calc_complements(matrix_t *A, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1) {
    if (A->rows == A->columns && A->rows > 1 && A->columns > 1) {
      answer = s21_create_matrix(A->columns, A->rows, result);
      if (answer == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < A->columns; j++) {
            double det = 0;
            matrix_t m = {0};
            s21_create_matrix(A->rows - 1, A->columns - 1, &m);
            matrixWithoutRowCol(i, j, A, &m, A->rows);
            s21_determinant(&m, &det);
            result->matrix[i][j] = pow(-1, (i + j)) * det;
            s21_remove_matrix(&m);
          }
        }
      } else {
        answer = INCORRECT;
      }
    } else {
      answer = CALC_ERROR;
    }
  } else {
    answer = INCORRECT;
  }
  return answer;
}

int s21_determinant(matrix_t *A, double *result) {
  int answer = OK;
  *result = 0;
  if (checkingMatrix(A) == 1) {
    if (A->rows == A->columns) {
      if (answer == OK) {
        *result = determinant22(A, A->rows);
      }
    } else {
      answer = CALC_ERROR;
    }
  } else {
    answer = INCORRECT;
  }
  return answer;
}

void matrixWithoutRowCol(int rows, int columns, matrix_t *A, matrix_t *result,
                         int n) {
  int offsetColumn, offsetRow = 0;
  result->rows = n - 1;
  result->columns = n - 1;
  for (int i = 0; i < n; i++) {
    if (i == rows) {
      offsetRow = 1;
    }
    offsetColumn = 0;
    for (int j = 0; j < n; j++) {
      if (j == columns) {
        offsetColumn = 1;
      }
      if (i != rows && j != columns) {
        result->matrix[i - offsetRow][j - offsetColumn] = A->matrix[i][j];
      }
    }
  }
}

double determinant22(matrix_t *matrix1, int n) {
  double result = 0;
  if (n == 1) {
    return matrix1->matrix[0][0];
  } else if (n == 2) {
    result = matrix1->matrix[0][0] * matrix1->matrix[1][1] -
             matrix1->matrix[1][0] * matrix1->matrix[0][1];
  } else {
    matrix_t temp = {0};
    int sign = 1;
    int err = s21_create_matrix(n, n, &temp);
    if (!err) {
      for (int i = 0; i < n; i++) {
        matrixWithoutRowCol(0, i, matrix1, &temp, n);
        result += sign * matrix1->matrix[0][i] * determinant22(&temp, n - 1);
        sign = -sign;
      }
    }
    s21_remove_matrix(&temp);
  }
  return result;
}

int s21_inverse_matrix(matrix_t *A, matrix_t *result) {
  int answer = OK;
  if (checkingMatrix(A) == 1) {
    if (A->rows == A->columns) {
      // answer = s21_create_matrix(A->rows, A->columns, result);
      if (answer == OK) {
        double det;
        answer = s21_determinant(A, &det);
        if (answer == OK) {
          if (fabs(det) > 1e-6) {
            matrix_t m1 = {0};
            if (answer == OK) {
              s21_calc_complements(A, &m1);
              s21_transpose(&m1, result);
              for (int i = 0; i < A->rows; i++) {
                for (int j = 0; j < A->rows; j++) {
                  result->matrix[i][j] /= det;
                }
              }
              s21_remove_matrix(&m1);
            }
          } else {
            answer = CALC_ERROR;
          }
        } else {
          answer = INCORRECT;
        }
      } else {
        answer = INCORRECT;
      }
    } else {
      answer = CALC_ERROR;
    }
  } else {
    answer = INCORRECT;
  }
  return answer;
}
