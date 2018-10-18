#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef
    struct No {
        void* info;
        struct No* prox;
        struct No* ant;
    }No;

typedef
    struct Lista {
        No* inicio;
        No* ult;
        int qtd;
        char* (*toString) (void*);
        int (*equals) (void*, void*);
    }Lista;

typedef
    struct Sistema {
        int quantasInc;
        Lista* lisInc;
        Lista* lisEqua;
        float** matrizCoeficientes;
        float* linhaResultados;
    }Sistema;

char* toStringStr (char* str) {
    return str;
}

int equalsStr (char* a, char* b)
{
    if(strcmp(a, b) == 0)
        return 1;

    return 0;
}

void limparMatriz (void** mat, int tamanho)
{
    int i;
    for(i = 0; i < tamanho; i++)
        free(*(mat + i));

    free(mat);
}

void limparNo (No* no)
{
    if(no -> prox == NULL)
        free(no);
    else
    {
        No* aux = no -> prox;
        limparNo(aux);
    }
}

void limparLista (Lista* lis)
{
    free(lis -> inicio);
    free(lis -> ult);

    free(lis);
}

void limparSistema(Sistema* sis)
{
    limparLista(sis -> lisInc);
    limparLista(sis -> lisEqua);

    limparMatriz((void**)sis -> matrizCoeficientes, sis -> quantasInc);

    free(sis -> linhaResultados);
    free(sis);
}

//inicia uma lista de string
Lista* novaListaStr ()
{
    Lista* lis = (Lista*)malloc(sizeof(Lista));

    lis->inicio = NULL;
    lis->ult = NULL;
    lis->qtd = 0;
    lis -> equals = (int(*)(void*,void*))&equalsStr;
    lis->toString = (char*(*)(void*))&toStringStr;

    return lis;
}

int tem (Lista* lis, void* rInfo)
{
    No* aux = lis -> inicio;

    while (aux != NULL)
    {
        if(lis -> equals(aux -> info, rInfo))
            return 1;

        aux = aux -> prox;
    }

    return 0;
}

//Insere no ultimo
int insere (Lista* lis, void* nInfo)
{
    if(tem(lis, nInfo))
        return 0;

    if(lis -> inicio == NULL)
    {
        lis -> inicio = (No*)malloc(sizeof(No));
        lis -> inicio -> info = nInfo;
        lis -> inicio -> prox = NULL;
        lis -> inicio -> ant = NULL;

        lis -> ult = lis -> inicio;
        lis -> qtd++;

        return 1;
    }

    No* aux = (No*)malloc(sizeof(No));

    aux -> info = nInfo;
    aux -> prox = NULL;
    aux -> ant = lis -> ult;

    lis -> ult -> prox = aux;
    lis -> ult = lis -> ult -> prox;
    lis -> qtd++;

    return 1;
}

int remover (Lista* lis, void* rInfo)
{
    No* aux = lis-> inicio;

    if(aux == NULL)
        return 0;

    if(lis -> equals(aux -> info, rInfo))
    {
        lis -> inicio = NULL;
        lis -> qtd--;

        return 1;
    }

    aux = aux -> prox;

    while(aux != NULL)
    {
        if(lis -> equals(aux -> info, rInfo))
        {
            aux -> ant -> prox = aux -> prox;
            free(aux);

            return 1;
        }
        aux = aux -> prox;
        lis -> qtd--;
    }

    return 0;
}

void printLis (Lista* lis)
{
    No* aux = lis -> inicio;

    char* ret = (char*)malloc(100000 * sizeof(char));
    strcpy(ret, "|");

    while(aux != NULL)
    {
        strcat(ret, lis-> toString(aux -> info));
        aux = aux -> prox;

        if(aux != NULL)
            strcat(ret, ", ");
    }
    strcat(ret, "|");

    printf("%s", ret);
}

void* getElemento (Lista* lis, int pos)
{
    if(lis -> inicio == NULL)
        return NULL;

    void* ret;
    int count = 0;

    No* aux = lis -> inicio;
    while(aux != NULL)
    {
        if(count == pos)
            break;

        aux = aux -> prox;
        count++;
    }

    if(count == pos)
    {
        ret = (void*)aux -> info;
        return ret;
    }

    ret = NULL;
    return ret;
}

int getPos(Lista* lis, void* nInfo)
{
    if(!tem(lis, nInfo))
        return -1;

    int cont = 0;

    No* aux = lis->inicio;
    while(aux != NULL)
    {
        if(lis -> equals(aux -> info, nInfo))
            break;
        aux = aux -> prox;

        cont++;
    }

    return cont;
}


//i é a linha e j é a coluna (forma complementar)
float** formaComp (float** matriz, int i, int j, int ordem)
{
    int a = 0;
    int b = 0;
    int c = 0;
    int d = 0;

    //alocando memória para a matriz que será utilizada
    float** mComplementar = (float**)malloc((ordem-1)*sizeof(float*));
    for(a = 0; a < ordem-1; a++)
        *(mComplementar+a) = (float*)malloc((ordem -1)*sizeof(float));

    //aqui teremos que percorrer a matriz original inteiramente
    // a é a linha da matriz original e b é a coluna;
    //c é a linha da matriz complemento e d é a coluna;
    for(a = 0; a < ordem; a++)
    {
        for(b = 0; b < ordem; b++)
        {
            if(a != i -1 && b != j -1)
            {
                *(*(mComplementar + c)+ d) = *(*(matriz + a) + b);

                //temos que resetar
                if(c == ordem -2)
                {
                    c = 0;
                    d++;

                    break;
                }
                else
                    c++;
            }
            else{}//o for incrementa a posição da matriz original então isso não faz nada
        }
        //end
        if(d == ordem -1)
            break;
    }

    return mComplementar;
}

//calcula determinante
float det (float** matriz, int ordem)
{
    float* linha;
    float** mAux;
    int i;
    int j=0;
    float determinante = 0;
    int aux;
    //matriz de ordem um, ou seja
    //det é igual a elemento
    if(ordem == 1)
        return *(*matriz);

    //usaremos a primeira linha da matriz para calcular
    linha = (float*)malloc(ordem * sizeof(float));
    for(i = 0; i < ordem; i++)
        *(linha + i) = *(*matriz + i);

    i = 0;

    for(j = 0; j < ordem; j++)
    {
        if((i + j) % 2 == 0)
            aux = 1;
        else
            aux = -1;

        mAux = formaComp(matriz,i+1 , j+1,ordem);
        determinante += linha[j] * aux * det(mAux, ordem-1);
        limparMatriz((void**)mAux, ordem -1);
    }

    free(linha);
    return determinante;
}

//retorna uma matriz feita por Cramer
float** matrizDeInc (float** coeficientes, float* resultados, int pos, int qtd)
{
    int i, j, a = 0;

    float** ret = (float**)malloc(qtd * sizeof(float*));
    for(i = 0; i < qtd; i++)
        ret[i] = (float*)malloc(qtd * sizeof(float));

    for(i = 0; i < qtd; i++)
    {
        for(j = 0; j < qtd; j++)
        {
            if(j == pos -1)
            {
                ret[i][j] = resultados[a];
                a++;
            }
            else
                ret[i][j] = coeficientes[i][j];
        }
    }

    return ret;
}

//recebe um ponteiro de sistema e devolve um vetor de incognitas
float* resolverSistema (Sistema* sis)
{
    int i;
    float detC;
    float detInc;
    float** aux;
    float* ret = (float*)malloc(sis -> quantasInc * sizeof(float));

    detC = det(sis -> matrizCoeficientes, sis -> quantasInc);
    if(detC == 0)
        return NULL;

    for(i = 0; i < sis -> quantasInc; i++)
    {
        aux = matrizDeInc(sis -> matrizCoeficientes, sis -> linhaResultados, i+1, sis -> quantasInc);
        detInc = det(aux, sis->quantasInc);

        limparMatriz((void**)aux, sis -> quantasInc);
        ret[i] = detInc/detC;
    }

    return ret;
}

void printarSistema (Sistema* sis)
{
    int i;
    Lista* lisE = sis -> lisEqua;

    printf("\n");
    for(i = 0; i < sis -> quantasInc; i++)
        printf("%s\n", (char*)getElemento(lisE, i));

    printf("");
}

void printResultado (Sistema* sis)
{
    int i;
    Lista* lis = sis -> lisInc;
    float* resolucao = resolverSistema(sis);

    if(resolucao == NULL)
    {
        printf("O sistema é SPI ou SI, ou seja não possui solução definida.");
        return;
    }

    printf("\nResposta dos coeficientes: ");
    for(i = 0; i < sis -> quantasInc; i++)
    {
        char * a = (char*)getElemento(lis, i);
        printf("%s = %.3f", a, resolucao[i]);

        if(i + 1 < sis -> quantasInc)
            printf(", ");
    }

    printf(".\n");
}

char* leArq(FILE* arq)
{
    fseek(arq, 0, SEEK_END);
    long int buffer_size = ftell(arq);
    fseek(arq, 0, SEEK_SET);
    char* concat = (char*)malloc(buffer_size);

    int i = 0;
    while (!feof(arq))
    {
        *(concat+i) = fgetc(arq);
        i++;
    }

    *(concat+i-1) = '\0';
    return concat;
}

Lista* separaEquacoes(char* nome, Sistema* sis)
{
    FILE* arq = fopen(nome, "r");
    char* texto = leArq(arq);

    Lista* lis = (Lista*)malloc(sizeof(Lista));
    lis->inicio = NULL;
    lis->ult = NULL;
    lis->qtd = 0;
    lis -> equals = (int(*)(void*,void*))&equalsStr;
    lis->toString = (char*(*)(void*))&toStringStr;

    char* s = (char*)malloc(sizeof(char)*1024);
    char* equacao = (char*)malloc(sizeof(char)*1024);
    strcpy(s, texto);
    equacao = strtok(s, "\n");
    while (equacao)
    {
        insere(lis, (void*)equacao);
        equacao = strtok(NULL, "\n");
    }
    fclose(arq);

    return lis;
}

void extraiCoeficientes(Sistema* sis, char* nome)
{
    //para deixar mais facil a resolucao:
    sis->lisEqua = separaEquacoes(nome, sis);
    sis -> quantasInc = sis -> lisEqua -> qtd;

    //inicializar:
    Lista* lis = novaListaStr();

    int i, j, n,
        inseriu = 0, // se o item foi inserido ou nao:
        posVariavel = 0, //variavel para contNolar quantas variaveis ja foram inseridas na lista
        cont = 0, //insere caracteres nas variáveis
        contC = 0, //insere caracteres nos coeficientes
        ehCo = 1;//variável para contNolar se um numero é ou nao coeficiente
    char c;//char lido da equação
    char* equacao = (char*)malloc(sizeof(char)*1024); //guarda uma das equacoes da lista
    char* result = (char*)malloc(sizeof(char)*100); //guarda o valor de um result
    char* coeficiente = (char*)malloc(sizeof(char)*100); //guarda o valor de um coeficiente
    char* varCm = (char*)malloc(sizeof(char)*100); //guarda temporariamente o nome de uma incognita
    char** variavel = (char**)malloc(sizeof(char*) * sis -> quantasInc);
    for(i = 0; i < sis -> quantasInc; i++)
    {
        variavel[i] = (char*)malloc(sizeof(char)*100);
        variavel[i][0] = '\0';
    }

    //nova matriz de coeficientes
    sis->matrizCoeficientes = (float**)malloc(sis->lisEqua->qtd * sizeof(float*));
    for(i = 0; i < sis->lisEqua->qtd; i++)
       sis->matrizCoeficientes[i] = (float*)malloc(sis->lisEqua->qtd * sizeof(float));

     //ler nomes das inc
    for(i = 0; i < sis -> lisEqua -> qtd; i++)
    {
        if(posVariavel >= sis -> quantasInc)
            break;
        equacao = (char*)getElemento(sis->lisEqua,i);
        for(n = 0; n < strlen(equacao); n++)
        {
            c = equacao[n];
            //se o char e uma letra
            if(c>=97 && c<=122)
            {
                variavel[posVariavel][cont] = c;
                cont++;
            }
            //se o char e um espaco (acaba o nome da var)
            else if (cont && c == 32)
            {
                variavel[posVariavel][cont] = '\0';
                inseriu = insere(lis, (void*)variavel[posVariavel]);
                cont = 0;

                if(inseriu)
                    posVariavel++;
            }
        }
    }

    sis -> lisInc = lis;
    cont = 0;

    //Inicializando os componentes que serao usados no sistema
    sis -> linhaResultados = (float*)malloc(sizeof(float)*sis -> quantasInc);
    sis -> matrizCoeficientes = (float**)malloc(sizeof(float*)*sis -> quantasInc);
    for(i = 0; i < sis -> quantasInc; i++)
        sis -> matrizCoeficientes[i] = (float*)malloc(sizeof(float) * sis -> quantasInc);


    //SEGUNDO FOR PARA EXTRAIR OS COEFICIENTES E RESULTADOS
    for(i = 0; i < sis->quantasInc; i++)
    {
        equacao = (char*)getElemento(sis->lisEqua,i);
        j = 0;
        ehCo = 1;
        for(n = 0; n < strlen(equacao); n++) //for que vai caracter por caracter da lista
        {
            c = equacao[n];

                //se char for um numero
                if(c >= 43 && c <= 57 && ehCo)
                {
                    coeficiente[contC] = c;
                    contC++;

                    //se o proximo for uma letra, termina a string do coeficiente
                    if(equacao[n+1]>=97){
                        coeficiente[contC] = '\0';
                        contC = 0;
                    }
                }
                //se o caracter for um numero do result
                else if(c >= 43 && c <= 57 && !ehCo)
                {
                    result[contC] = c;
                    contC++;
                }
                //se eh o ultimo num da equacao
                if(n==strlen(equacao)-1)
                {
                    result[contC] = '\0';
                    sis->linhaResultados[i] = strtof(result, NULL);
                    contC = 0;

                }

                //char é letra
                else if(c>=97 && c<=122)
                {
                    varCm[cont] = c;
                    cont++;
                }
                //se for um igual sabemos que os próximos valores serão resultados
                else if(c==61)
                    ehCo = 0;
                //se for um espaço em branco
                else
                {   //se for um espaço depois de uma variavel insere o coeficiente
                    if(ehCo && cont)
                    {
                        varCm[cont] = '\0';

                        j = getPos(lis, varCm);
                        sis->matrizCoeficientes[i][j] = strtof(coeficiente, NULL);
                        cont = 0;
                        contC = 0;
                        varCm = (char*)malloc(sizeof(char)*100);
                    }
                }
            }
        }

        free(equacao);
        free(coeficiente);
        free(result);
        free(varCm);
}

void resolveSistema ()
{
    Sistema sis;
    sis.lisEqua = NULL;
    sis.lisInc = NULL;
    sis.matrizCoeficientes = NULL;
    sis.quantasInc = 0;

    char* nome = (char*)malloc(sizeof(char)*100);

    printf("Digite o nome do arquivo: ");
    scanf("%s", nome);

    nome[strlen(nome)] = '\0';
    extraiCoeficientes(&sis, nome);

    printf("Sistema | \n");
    printf("        v  \n");
    printarSistema(&sis);
    printResultado(&sis);

    limparSistema(&sis);
}

int main()
{
    int modo =  -1;
    int fim = 0;

    printf("Este e o resolvedor de sistemas lineares. insere a opção desejada. \n");
    do{
        printf("Digite 0: Encerrar o programa\n");
        printf("Digite 1: Solucionar um sistema\n");
        scanf("%d", &modo);

        switch (modo)
        {
            case 0: system("cls"); return 0; break;
            case 1: system("cls"); resolveSistema(); break;
            default: break;
        }

        printf("\nDeseja sair?(N = 0/ S = 1)\n");
        scanf("%d", &fim);
        system("cls");
    }while(!fim);

    return  0;
}
