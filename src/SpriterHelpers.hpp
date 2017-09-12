#ifndef SPRITER_HELPERS_HPP
#define SPRITER_HELPERS_HPP

#include <spriterengine/override/filefactory.h>
#include <spriterengine/override/imagefile.h>
#include "jsonspriterfiledocumentwrapper.h"
#include <iostream>
#include <functional>

using namespace std;
using namespace SpriterEngine;

class HaskellSprite {};

class SpriterImageFile : public ImageFile
{
public:
    SpriterImageFile(HaskellSprite* sprite,
                     function<void()> render,
                     const string& filename,
                     point pivot) :
        sprite(sprite),
        ImageFile(filename, pivot),
        render(render) {
    }

    void renderSprite(UniversalObjectInterface *spriteInfo) override {
        render();
    }

private:
    HaskellSprite* sprite;
    function<void()> render = nullptr;
};

class SpriterFileFactory : public FileFactory
{
public:
    SpriterFileFactory(function<HaskellSprite*(const char*, double, double)> imageLoad,
                       function<void()> render) :
        imageLoad(imageLoad), render(render) {
    }

    ImageFile *newImageFile(const string& filename,
                            point pivot,
                            atlasdata atlasData) override {
        HaskellSprite* sprite = imageLoad(filename.c_str(), pivot.x, pivot.y);
        return new SpriterImageFile(sprite, render, filename, pivot);
    }

	SpriterFileDocumentWrapper* newSconDocumentWrapper() override {
        return new JSONSpriterFileDocumentWrapper();
    }

private:
    function<HaskellSprite*(const char*, double, double)> imageLoad;
    function<void()> render;
};

#endif
