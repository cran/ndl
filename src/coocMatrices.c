/*
 ============================================================================
 Name        : cooccurrence.c
 Author      : Petar Milin
 Version     : 1.0
 Description : This is an auxiliary function for the ndl package in R.
 ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NO_WORDS 500000
#define MAX_STRING_LENGTH 20010
#define MAX_NO_CUES_OUTCOMES 20010
#define input_file "ndl_410025912.txt"
#define output_file1 "coocCuesOutcomes_410025912.txt"
#define output_file2 "rows_410025912.txt"
#define output_file3 "columns_410025912.txt"
#define output_file4 "coocCues_410025912.txt"
#define par_file "ndl_par_410025912.txt"

void cooc()
{
	char **name_matrix1 = (char**) malloc(MAX_NO_WORDS*sizeof(char*));
	char **name_matrix2 = (char**) malloc(MAX_NO_WORDS*sizeof(char*));
	int matrix_size1 = 0, matrix_size2 = 0;
	int T1[MAX_NO_CUES_OUTCOMES], nfeatures1;
	int T2[MAX_NO_CUES_OUTCOMES], nfeatures2;

	char col2[MAX_STRING_LENGTH], col3[MAX_STRING_LENGTH], parameters[50], *p1, *p2, *d1;
	int i, j, k, freq, no_lines = 0;

	/* The following can read in parameters for e.g. "duplicates=TRUE/FALSE" */
	FILE *par;
        par = fopen(par_file, "rb");
	if (par == NULL) {printf("ERROR: NO PARAMETER FILE \'ndl_par_410025912.txt\'\n"); exit(3);}
        fscanf(par, "%s", &parameters);
	fclose(par);

        /* if(strcmp(parameters,"duplicates=TRUE") == 0)
	   printf("%s\n", parameters); */

	FILE *dat;
	dat = fopen(input_file, "rb");
	if (dat == NULL) {printf("ERROR: NO INPUT FILE \'ndl_410025912.txt\'\n"); exit(3);}
	fgets(col2, MAX_STRING_LENGTH, dat); // READS AND IGNORES HEADERS

	// CREATES A DICTIONARY FOR INPUT n-GRAMS
	while (1)
	{
		fscanf(dat, "%i%s%s", &freq, col2, col3);
		if (feof(dat)) break;
		strcat(col2, "_");
		p1 = col2;
		while ((p2 = strchr(p1, '_')) != NULL)
		{
			p1[p2-p1] = 0;
			for (i = 0; i < matrix_size1; i++)
			if (strcmp(name_matrix1[i], p1) == 0)
				break;
			if (i == matrix_size1)
			{
				name_matrix1[i] = (char*) malloc(strlen(p1)+1);
				strcpy(name_matrix1[i], p1);
				if (++matrix_size1 == MAX_NO_WORDS)
				{
					printf("ERROR: INPUT TOO LARGE\n");
					exit(1);
				}
			}
			p1 = p2 + 1;
		}
		strcat(col3, "_");
		p1 = col3;
		while ((p2 = strchr(p1, '_')) != NULL)
		{
			p1[p2-p1] = 0;
			for (i = 0; i < matrix_size2; i++)
			if (strcmp(name_matrix2[i], p1) == 0)
				break;
			if (i == matrix_size2)
			{
				name_matrix2[i] = (char*) malloc(strlen(p1)+1);
				strcpy(name_matrix2[i], p1);
				if (++matrix_size2 == MAX_NO_WORDS)
				{
					printf("ERROR: INPUT TOO LARGE\n");
					exit(1);
				}
			}
			p1 = p2 + 1;
		}
	}
	rewind(dat);
	fgets(col2, MAX_STRING_LENGTH, dat);
	name_matrix1 = (char**) realloc(name_matrix1, matrix_size1*sizeof(char*));
	name_matrix2 = (char**) realloc(name_matrix2, matrix_size2*sizeof(char*));
	int *M = (int*) calloc(matrix_size1*matrix_size2, sizeof(int));
	int *M1 = (int*) calloc(matrix_size1*matrix_size1, sizeof(int));
        int dupl1[matrix_size1], dupl2[matrix_size2];

	while (1)
        {  
		fscanf(dat, "%i%s%s", &freq, col2, col3);
		no_lines++;
		if (feof(dat)) break;
		strcat(col2, "_");
		p1 = col2;
		nfeatures1 = 0;
		while ((p2 = strchr(p1, '_')) != NULL)
		{
			p1[p2-p1] = 0;
			for (j = 0; j < matrix_size1; j++)
			  if (strcmp(name_matrix1[j], p1) == 0)
			    break;
			for (k = 0; k < nfeatures1; k++)
			  if (T1[k] == j)
			    { if(strcmp(parameters,"duplicates=TRUE") == 0)
				dupl1[k]++;
			      else
				dupl1[k]=1;
			      break;
			    }
			if (k == nfeatures1)
			  { dupl1[k] = 1;
			    T1[nfeatures1++] = j;
			  }
			p1 = p2 + 1;
		}
		strcat(col3, "_");
		p1 = col3;
		nfeatures2 = 0;
		while ((p2 = strchr(p1, '_')) != NULL)
		{
			p1[p2-p1] = 0;
			for (j = 0; j < matrix_size2; j++)
			  if (strcmp(name_matrix2[j], p1) == 0)
			    break;
			for (k = 0; k < nfeatures2; k++)
			  if (T2[k] == j)
			    { if(strcmp(parameters,"duplicates=TRUE") == 0)
                                dupl2[k]++;
			      else
				dupl2[k] = 1;
			      break;
			    }
			if (k == nfeatures2)
			  { dupl2[k] = 1;
			    T2[nfeatures2++] = j;
			  }
			p1 = p2 + 1;
		}
		for (i = 0; i < nfeatures1; i++)
		  for (j = 0; j < nfeatures2; j++)
		    M[T1[i]*matrix_size2+T2[j]] += freq * dupl1[i] * dupl2[j];
		for (i = 0; i < nfeatures1; i++)
		  for (j = 0; j < nfeatures1; j++)
		    M1[T1[i]*matrix_size1+T1[j]] += freq * dupl1[i] * dupl1[j];
 	}
	fclose(dat);

	dat = fopen(output_file1, "wb");
	for (i = 0; i < matrix_size1; i++)
	{
		for (j = 0; j < matrix_size2; j++)
			fprintf(dat, "%i ", M[i*matrix_size2+j]);
		fprintf(dat, "\n");
	}
	fclose(dat);

	dat = fopen(output_file2, "wb");
	for (i = 0; i < matrix_size1; i++)
		fprintf(dat, "%s\n", name_matrix1[i]);
	fclose(dat);

	dat = fopen(output_file3, "wb");
	for (i = 0; i < matrix_size2; i++)
		fprintf(dat, "%s\n", name_matrix2[i]);
	fclose(dat);

	dat = fopen(output_file4, "wb");
	for (i = 0; i < matrix_size1; i++)
	{
		for (j = 0; j < matrix_size1; j++)
			fprintf(dat, "%i ", M1[i*matrix_size1+j]);
		fprintf(dat, "\n");
	}
	fclose(dat);

	for (i = 0; i < matrix_size1; i++)
		free(name_matrix1[i]);
	free(name_matrix1);
	for (i = 0; i < matrix_size2; i++)
		free(name_matrix2[i]);
	free(name_matrix2);
	free(M);
	free(M1);
}

