
#include <spriterengine/spriterengine.h>

#include <spriterengine/override/filefactory.h>

#include <spriterengine/global/settings.h>

#include "SpriterHelpers.hpp"

using namespace SpriterEngine;

extern "C" {
SpriterModel * inline_c_Main_0_f3ce6667b7e7fb2a80dfc9627ad710e87d6e1438(char * modelPath_inline_c_0, HaskellSprite * (* imgloader_inline_c_1)(const char *, double , double ), void (* renderer_inline_c_2)()) {
return (
            new SpriterModel(
                modelPath_inline_c_0,
                new SpriterFileFactory(
                        imgloader_inline_c_1,
                        renderer_inline_c_2
                )
          )
        );
}

}

extern "C" {
EntityInstance * inline_c_Main_1_73688703005a2eaabe5d481a895e59107107096b(SpriterModel * model_inline_c_0, char * entityName_inline_c_1) {
return ( model_inline_c_0->getNewEntityInstance(entityName_inline_c_1) );
}

}

extern "C" {
void inline_c_Main_2_d81c12f586a59cc374d5ed7bbbb3eff989c03ebc(EntityInstance * ptr_inline_c_0, char * animName_inline_c_1) {
 ptr_inline_c_0->setCurrentAnimation(animName_inline_c_1) ;
}

}

extern "C" {
void inline_c_Main_3_0904dee9f665a3497d4479a5e816f6a31739d10c() {
 Settings::setErrorFunction(Settings::simpleError); ;
}

}

extern "C" {
void inline_c_Main_4_52748ac8bbe7c929c18d02e526b7ebd8be6b3f31(EntityInstance * entityInstance_inline_c_0, double cTimeStep_inline_c_1) {

                 //cout << "yop" << endl;
                 auto ent = entityInstance_inline_c_0;
                 //cout << "yip" << endl;
                 ent->setTimeElapsed(cTimeStep_inline_c_1);
                 //cout << "yup" << endl;
                 ent->render();
                 //cout << "yap" << endl;
             
}

}
