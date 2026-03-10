#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <assert.h>
#include <lo/lo.h>
#include <stdint.h>
#include <netdb.h>

#include "scheduler.h"

void
osc_send(char *host, char *port, char *path, char *types, ...)
{
   va_list ap;
   va_start(ap, types);

   lo_message msg = lo_message_new();
   lo_message_add_varargs(msg, types, ap);

   lo_address addr = lo_address_new(host, port);
   lo_send_message(addr, path, msg);

   va_end(ap);
}

void* osc_create_message() { return lo_message_new(); }
void osc_free_message(void *msg) { lo_message_free(msg); }
void osc_message_add_int32(void *m, int32_t v) { lo_message_add_int32(m, v); }
void osc_message_add_int64(void *m, int64_t v) { lo_message_add_int64(m, v); }
void osc_message_add_string(void *m, const char *str) { lo_message_add_string(m, str); }
void osc_message_add_char(void *m, const char c) { lo_message_add_char(m, c); }
void osc_message_add_double(void *m, const double v) { lo_message_add_double(m, v); }

void
osc_send_message(void *msg, const char *host, const char *port, const char *path)
{
   lo_address addr = lo_address_new(host, port);
   lo_send_message(addr, path, msg);
}

// Scheduling

typedef struct {
   void *msg;
   char *host;
   char *port;
   char *path;
} osc_arg_t;

void
exec_osc_message(void *argP)
{
   osc_arg_t *arg = (osc_arg_t*) argP;
   osc_send_message(arg->msg, arg->host, arg->port, arg->path);
   free(arg->host);
   free(arg->port);
   free(arg->path);
   free(arg);
}

void
schedule_osc_message(scheduler_t *sch, struct timespec tm,
      const char *host, const char *port, const char *path,
      void *msg)
{
   osc_arg_t *arg = (osc_arg_t*) malloc(sizeof(osc_arg_t));
   arg->host = strdup(host);
   arg->port = strdup(port);
   arg->path = strdup(path);
   schedule_event(sch, tm, exec_osc_message, arg);
}

lo_message
osc_receive(unsigned short port)
{
   int sock, retlen = 0;
   struct sockaddr_in server;
   char buf[2048];

   if ((sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
      perror("socket");
      return NULL;
   }

   memset((void*)&server, 0, sizeof(server));
   server.sin_family      = AF_INET;
   server.sin_port        = htons(port);
   server.sin_addr.s_addr = INADDR_ANY;

   if (bind(sock, (struct sockaddr *)&server, sizeof(server)) < 0) {
      perror("bind");
      return NULL;
   }

   retlen = recvfrom(sock, buf, sizeof(buf), 0, NULL, NULL);
   if (retlen < 0) {
      perror("recvfrom");
      return NULL;
   }

   close(sock);

   lo_message msg = lo_message_deserialise(buf, retlen, NULL);
   char *path = lo_get_path(buf, retlen);
   printf("%s: ", path);
   lo_message_pp(msg);
   return msg;
}

void*
osc_server_init(char *port)
{
   return lo_server_new(port, NULL);
}
