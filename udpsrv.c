#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char **argv)
{
   int s, retlen = 0;
   unsigned namelen, client_address_size;
   struct sockaddr_in client, server;
   char buf[2048];

   if ((s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0)
   {
      perror("socket");
      exit(1);
   }

   memset((void*)&server, 0, sizeof(server));
   server.sin_family      = AF_INET;
   server.sin_port        = htons(20202);
   server.sin_addr.s_addr = INADDR_ANY;

   if (bind(s, (struct sockaddr *)&server, sizeof(server)) < 0)
   {
      perror("bind");
      exit(2);
   }

   namelen = sizeof(server);
   if (getsockname(s, (struct sockaddr *) &server, &namelen) < 0)
   {
      perror("getsockname");
      exit(3);
   }

   printf("Port assigned is %d\n", ntohs(server.sin_port));

   client_address_size = sizeof(client);

   while (1) {
      retlen = recvfrom(s, buf, sizeof(buf), 0, (struct sockaddr *) &client,
            &client_address_size);
      if (retlen < 0)
      {
         perror("recvfrom()");
         exit(4);
      }

      printf("Received message from domain %s port %d internet\
            address %s\n",
            (client.sin_family == AF_INET?"AF_INET":"UNKNOWN"),
            ntohs(client.sin_port),
            inet_ntoa(client.sin_addr));

      for (unsigned i = 0; i < retlen; i ++)
         printf("%02X ", buf[i]);
      puts("");
   }

   close(s);
}


