
#include <spriterengine/spriterengine.h>

#include <spriterengine/override/filefactory.h>

#include <spriterengine/global/settings.h>

#include "SpriterHelpers.hpp"

using namespace SpriterEngine;

extern "C" {
SpriterModel * inline_c_Main_0_4ceca4cc758f5f7fd8ee1939d2a3c25ce9a2a266(char * modelPath_inline_c_0, HaskellSprite * (* imgloader_inline_c_1)(const char *, double , double ), void (* renderer_inline_c_2)(HaskellSprite *, SpriteState *)) {
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
void inline_c_Main_4_89e6aee5670c9e6338390bb074524209cb06fa28(EntityInstance * entityInstance_inline_c_0, double cTimeStep_inline_c_1) {

                 auto ent = entityInstance_inline_c_0;
                 ent->setTimeElapsed(cTimeStep_inline_c_1);
                 ent->render();
             
}

}
