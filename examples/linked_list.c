#include<stdio.h>
#include<stdlib.h> 


struct node
{
     struct node *previous;
     int data;
     struct node *next;
}; 

void insert_beginning(int value, struct node **head, struct node **last)
{
     struct node *var;
     struct node *temp;
     struct node *temp2;
     var=(struct node *)malloc(24);
//	var=malloc(24);
 //    printf("size node = %d", sizeof(struct node)); //size is 24 bytes
     var->data = value;
     if(*head==0)
     {
	 printf("head is 0\n");
         var->previous=0;
         var->next=0;
         *head = var;
 //        head->previous=0;
 //        head->next=0;
         last = head;
     }
     else
     {
 	 printf("head is not 0\n");
         temp = var;
         temp->previous=0;
         temp->next = *head;
         temp2 = *head;
	 temp2->previous = temp;
         *head = temp;
     }
} 
/*
void insert_end(int value, struct node *head, struct node *tail)
{
     struct node *var,*temp;
     var=(struct node *)malloc(sizeof(struct node));
             var->data=value;
     if(head==NULL)
     {
          head=var;
          head->previous=NULL;
          head->next=NULL;
          last=head;
     }
     else
     {
         last=head;
         while(last!=NULL)
         {
             temp=last;
             last=last->next;
         }
     last=var;
     temp->next=last;
     last->previous=temp;
     last->next=NULL;
     }
}   

int insert_after(int value, int loc, struct node *head, struct node *tail)
{
     struct node *temp,*var,*temp1;
     var=(struct node *)malloc(sizeof(struct node));
     var->data=value;
         if(head==NULL)
     {
           head=var;
           head->previous=NULL;
           head->next=NULL;
     }
     else
     {
           temp=head;
           while(temp!=NULL && temp->data!=loc)
           {
                 temp=temp->next;
           }
           if(temp==NULL)
           {
                printf("\n%d is not present in list ",loc);
           }
           else
           {
           temp1=temp->next;
           temp->next=var;
           var->previous=temp; 
          var->next=temp1;
           temp1->previous=var;
           }
     }
     last=head;
     while(last->next!=NULL)
     {
           last=last->next;
     }
}   
int delete_from_end(struct node *head, struct node *tail)
{
      struct node *temp;
      temp=last;
      if(temp->previous==NULL)
      {
           free(temp);
           head=NULL;
           last=NULL;
           return 0;
      }
      printf("\nData deleted from list is %d \n",last->data);
      last=temp->previous;
      last->next=NULL;
      free(temp);
      return 0;
} 

int delete_from_middle(int value, struct node *head, struct node *tail)
{
    struct node *temp;
    struct node *var;
    struct node *t;
    struct node *temp1;
    temp=head;

    while(temp!=NULL)
    {
        if(temp->data == value)
        {
             if(temp->previous==NULL)
             {
                  free(temp);
                  head=NULL;
                  last=NULL;
                  return 0;
             }
             else
             {
                  var->next=temp1;
                  temp1->previous=var;
                  free(temp);
                  return 0;
             }
        }
        else
        {
              var=temp;
              temp=temp->next;
              temp1=temp->next;
        }
    }
    printf("data deleted from list is %d",value);
}  
*/
void display(struct node **head, struct node **last)
{
     struct node *temp;
     temp=*head;
     if(temp==0)
      {
         printf("List is Empty");
      }
     while(temp!=0)
     {
          printf("-> %d ",temp->data);
          temp=temp->next;
     }
} 

int main()
{
    int value;
    int i; 
    int loc;
    struct node *head;
    struct node *last;

    head = 0;

    printf("Select the choice of operation on link list");
    printf("\n1.) insert at beginning\n2.) insert at at\n3.) insert at middle");
    printf("\n4.) delete from end\n5.) reverse the link list\n6.) display list\n7.)exit");
    while(1)
    {
          printf("\n\nenter the choice of operation you want to do ");
          scanf("%d",&i);

	if (i == 1){
	//	printf("enter the value you want to insert in node ");
	//	scanf("%d",&value);
		insert_beginning(8, &head, &last);
		display(&head, &last);
	} else {if (i == 2){
		printf("enter the value you want to insert in node at last ");
		/*scanf("%d",&value);
		insert_end(value, head, last);
		display(head, last); */
	} else {if (i == 3){
		 printf("after which data you want to insert data ");
		/* scanf("%d",&loc);
		 printf("enter the data you want to insert in list ");
		 scanf("%d",&value);
		 insert_after(value,loc, head, last);
		 display(head, last); */
	} else {if (i == 4){
	       /*  delete_from_end(head, last);
                 display(head, last); */
	} else {if (i == 5){
                 printf("enter the value you want to delete");
            /*     scanf("%d",value);
                 delete_from_middle(value, head, last);
                 display(head, last);*/
	} else {if (i == 6){
	       //  display(head, last);
	} else {if (i == 7){
    		//printf("\n\n%d",last->data);
    		//display(head, last);
		return 0;
	} 
	}}}}}}
    }

}
