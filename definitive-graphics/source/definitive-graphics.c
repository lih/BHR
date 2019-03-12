#include <gtk/gtk.h>
#include <vte/vte.h>
#include "definitive-graphics.h"

#if !(GTK_CHECK_VERSION(3,16,0))
void gtk_label_set_xalign(GtkWidget* lbl,gfloat f) { return; }
GtkWidget* gtk_gl_area_new() {
  return gtk_label_new("GtkGLArea is not available with this version of GTK");
}
void gtk_gl_area_queue_render(GtkWidget* _) { return; }
#endif

void definitive_gtkConnectSignal(void* inst,char const* type,void (*c_handler)(void),void* data) {
  g_signal_connect(inst,type,c_handler,data);
}
int definitive_entry_get_cursor(GtkEntry* entry) {
  int ret;
  g_object_get_property(G_OBJECT(entry),"cursor-position",&ret);
  return ret;
}
GPid definitive_vte_terminal_fork_command(VteTerminal* term,char** argv) {
  GPid ret;
  GError* err = NULL;
  if(
#if VTE_CHECK_VERSION(0,38,0)
    vte_terminal_spawn_sync(term,VTE_PTY_DEFAULT,NULL,argv,NULL,G_SPAWN_DEFAULT,NULL,NULL,&ret,NULL,&err)
#else
    vte_terminal_fork_command_full(term,VTE_PTY_DEFAULT,NULL,argv,NULL,G_SPAWN_DEFAULT,NULL,NULL,&ret,&err)
#endif
     )
    return ret;
  else {
    printf("Error while spawning VteTerminal: %s\n",err->message);
    return 0;
  }
}

void definitive_forGList(GList* (*alloc)(),void (*f)(gpointer data)) {
  GList* l = alloc();
  GList* cur = l;
  while(cur != NULL) {
    f(cur->data);
    g_free(cur->data);
    cur = cur->next;
  }
  g_slist_free(l);
}

#define DEFPROPERTYTYPE(t,tag) \
  t definitive_g_object_get_##tag (gpointer obj, char const* prop) {    \
    t ret; g_object_get(obj,prop,&ret,NULL); return ret; }              \
  void definitive_g_object_set_##tag (gpointer obj,char const* prop,t v) { \
    g_object_set(obj,prop,v,NULL); }

DEFPROPERTYTYPE(int,i);
DEFPROPERTYTYPE(float,f);

#define DEFGETTER(n,r,t,p) r definitive_##n##_get_##p (t* e) { return e->p; }

DEFGETTER(gdkEvent,gdouble,GdkEventButton,x);
DEFGETTER(gdkEvent,gdouble,GdkEventButton,y);
DEFGETTER(gdkEvent,guint,GdkEventButton,button);
DEFGETTER(gdkEvent,guint32,GdkEventButton,time);
DEFGETTER(gdkEvent,guint,GdkEventKey,keyval);
DEFGETTER(fileFilterInfo,const gchar*,GtkFileFilterInfo,filename);
