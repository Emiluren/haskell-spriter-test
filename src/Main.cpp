
#include <spriterengine/spriterengine.h>

#include <spriterengine/override/filefactory.h>

#include <spriterengine/global/settings.h>

#include "SpriterHelpers.hpp"

using namespace SpriterEngine;

extern "C" {
SpriterModel * inline_c_Main_0_81169fea54450e7239a3756562373020c6dac348(char * modelPath_inline_c_0) {
return ( new SpriterModel(modelPath_inline_c_0, new SpriterFileFactory()) );
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
void inline_c_Main_3_5b59f388aec97892e0e4f9e43b4d357f1c8e1b97() {
 Settings::setErrorFunction(Settings::simpleError);;
}

}

extern "C" {
void inline_c_Main_4_1418bcf5c7196d54f30f71e042c5d9e9765017c0(void (* printWithMsg_inline_c_0)(double ), void (* printWithMsg_inline_c_1)(double )) {

         printWithMsg_inline_c_0(4.0);
         printWithMsg_inline_c_1(8.0);
     
}

}
