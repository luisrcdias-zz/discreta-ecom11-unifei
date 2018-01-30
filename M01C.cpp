#include <stdio.h>
#include <stdbool.h>
//Códigos Recursivos
int numPar;


int i;

//Os Exercícios entregues durante a aula não continham as funćões Recursivas, apenas iterativas.
bool primo (int n, int k) {

    if (n <= 1)
        return false;
    else if (k >= n)
        return true;
    else if (n % k == 0)
        return false;
    else {
        k++;
        primo(n, k);
    }

}



void vetPrimo (int * vetor, int num, int i, int x) {

    if (x < num) {

        if (primo(i, 2)) {
            vetor[x] = i;
            x++;
        }

        i++;
        vetPrimo(vetor, num, i, x);

    }

}


bool auxCalcGoldbach(int num1, int j, int * vetor) {

    if(num1 + vetor[j] == numPar) {
        printf("%d + %d = %d\n", num1, vetor[j], numPar);
        return true;
    }
    else if(j == numPar)
        return false;
    else {
        j++;
        auxCalcGoldbach(num1, j, vetor);
    }

}

void calcGoldbach(int i, int * vetor) {

    if(auxCalcGoldbach(vetor[i], 0, vetor)){
        return;
    }
    else if(i == numPar){
        printf("Numero invalido.\n");
        return;
    }
    else {
        i++;
        calcGoldbach(i, vetor);
    }

}

void goldbach (int * vetor) {

    vetPrimo(vetor, numPar, 2, 0);

    calcGoldbach(0, vetor);

}




//Códigos Iterativos
//Os primeiros dois exercícios iterativos estavam com erros na lógica do algoritmo
//Apresentavam erro referente ao tratamento de 0 como número primo.
//O terceiro exercicio iterativo não foi feito em aula.
bool primoite(int num){
  int i;
  for(i=2;i<num;i++){
    if(num%i==0){break;}}
    if(i==num){return true;}else{return false;}
  }



void nPrimosite(int *vetor,int num){
int i=0,x=0;
while(x<num){
  if(primoite(i)){
    vetor[x]=i;
    x++;
  }
  i++;
}
}

void goldbachite(int *vetor, int num){
  nPrimosite(vetor,num);
  int i,j, num1, num2;

  for(i=0;i<num;i++){
    num1=vetor[i];
    for(j=i;j<num;j++){
      num2=vetor[j];
      if(num1+num2 == num){
        printf("%d + %d = %d\n",num1, num2, num );
        return;
      }
    }
  }
  printf("Numero Invalido");
}

int main ()
{
  int vetor[10000],option,a,k;
  printf ("Escolha uma opcao:\n1-Eh Primo Iterativo || 2- Eh Primo Recursivo n\n3-Listar n Primos Iterativo || 4- Listar n Primos Recursivo \n 5-Goldbach Iterativo  || 6-Goldbach Recursivo n\n");
  scanf("%d", &option);

  switch ( option )
  {
  case 1:
  printf("Verificar se n eh primo(Iterativo)\n n=");
scanf("%d",&a);
  if(primoite(a)==true){
    printf("\nEh Primo\n");
  }else{
    printf("\nNao eh primo\n");
  }
   break;
   case 2:
   printf("Verificar se n eh primo(Recursivo)\n n=");
 scanf("%d",&a);
   if(primo(a,2)==true){
     printf("\nEh Primo\n");
   }else{
     printf("\nNao eh primo\n");
   }
    break;
  case 3:
  printf("Listar n primos(Iterativo)\n n= \n");
  scanf("%d",&a);
  nPrimosite(vetor,a);
for(k=0;k<a;k++){
  printf("%d,",vetor[k]);
}
 break;
 case 4:
 printf("Listar n primos(Recursivo)\n n= \n");
 scanf("%d",&a);
 vetPrimo(vetor,a,2,0);
for(k=0;k<a;k++){
 printf("%d,",vetor[k]);
}
break;
  case 5:
  printf("Conjectura de goldbach para n(Iterativo)\n n= \n");
  scanf("%d",&a);
  goldbachite(vetor,a);
   break;
   case 6:
   printf("Conjectura de goldbach para n(Recursivo)\n n= \n");
   scanf("%d", &numPar);
           int vet[numPar];
           goldbach(vet);
}
}
