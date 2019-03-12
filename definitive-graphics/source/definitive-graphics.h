#define FIELD_LENS(f) lens f (\x y -> x { f = y })
#define DEFFIELD_LENS(l) l = FIELD_LENS(_##l)
#define TRAITINSTANCE(t,p) IFN_##t(instance DynamicProperty w a p => DynamicProperty (t w) a p where { property p = traitValue.property p })
#define TRAITINSTANCES(p) TRAITINSTANCE(DynProps,p); TRAITINSTANCE(Clickable,p); TRAITINSTANCE(Hideable,p); TRAITINSTANCE(BoxChild,p); TRAITINSTANCE(WProps,p); TRAITINSTANCE(Focusable,p); TRAITINSTANCE(KeyboardEnabled,p); TRAITINSTANCE(GridChild,p)
#define FOREIGN(n,t) foreign import ccall #n n :: t
#define DEFPROP(n,p) data p = p ; TRAITINSTANCES(p) ; n :: DynamicProperty w a p => Lens a a w w ; n = property p
#define HASPROP(p,w,t,v...) instance DynamicProperty (w) (t) p where property p = v
#define DEFCALLBACK(tag,t) foreign import ccall "wrapper" callback_##tag :: (t) -> IO (FunPtr (t))

#define IFN_DynProps(x...) x
#define IFN_WProps(x...) x
#define IFN_Hideable(x...) x
#define IFN_BoxChild(x...) x
#define IFN_Clickable(x...) x
#define IFN_Focusable(x...) x
#define IFN_KeyboardEnabled(x...) x
#define IFN_GridChild(x...) x

#define GTK_ORIENTATION_HORIZONTAL 0x0
#define GTK_ORIENTATION_VERTICAL 0x1

#define GTK_POS_BOTTOM              0x3
#define GTK_POS_TOP                 0x2
#define GTK_POS_LEFT                0x0
#define GTK_POS_RIGHT               0x1

#define GTK_PACK_START              0x0
#define GTK_PACK_END                0x1

#define GTK_ICON_SIZE_BUTTON        0x4
#define GTK_ICON_SIZE_MENU          0x1
#define GTK_ICON_SIZE_SMALL_TOOLBAR 0x2
#define GTK_ICON_SIZE_LARGE_TOOLBAR 0x3
#define GTK_ICON_SIZE_DND           0x5
#define GTK_ICON_SIZE_DIALOG        0x6

#define GTK_ACCEL_VISIBLE           0x1
#define GTK_ACCEL_LOCKED            0x2
#define GTK_ACCEL_MASK              0x7

#define GDK_EXPOSURE_MASK           0x2
#define GDK_POINTER_MOTION_MASK     0x4
#define GDK_BUTTON_MOTION_MASK      0x10
#define GDK_BUTTON_PRESS_MASK       0x100
#define GDK_BUTTON_RELEASE_MASK     0x200

#define GTK_FILE_CHOOSER_ACTION_OPEN 0x0
#define GTK_FILE_CHOOSER_ACTION_SAVE 0x1
#define GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER 0x2
#define GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER 0x3

#define GTK_FILE_FILTER_FILENAME 0x1
#define GTK_FILE_FILTER_URI 0x2
#define GTK_FILE_FILTER_DISPLAY_NAME 0x4
#define GTK_FILE_FILTER_MIME_TYPE 0x8

#define GTK_DIALOG_MODAL 0x1
#define GTK_DIALOG_DESTROY_WITH_PARENT 0x2
#define GTK_DIALOG_USE_HEADER_BAR 0x4

#define GDK_RELEASE_MASK 0x40000000
