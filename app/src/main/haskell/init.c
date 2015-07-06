#include <jni.h>
#include <HsFFI.h>
#include <SDL.h>

extern void __stginit_HaskellActivity(void);
extern void hsMain(void);

int
main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    // GHC-specific
    hs_add_root(__stginit_HaskellActivity);
    hsMain();
}

//JNIEXPORT void Java_org_libsdl_app_SDLActivity_nativeQuit(JNIEnv* env, jclass cls, jobject obj)
JNIEXPORT void JNICALL
JNI_OnUnload( JavaVM *vm, void *pvt )
{
    hs_exit();
}
