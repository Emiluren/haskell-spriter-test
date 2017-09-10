#ifndef SPRITER_HELPERS_HPP
#define SPRITER_HELPERS_HPP

#include <spriterengine/override/filefactory.h>
#include <spriterengine/override/imagefile.h>
#include "jsonspriterfiledocumentwrapper.h"

using namespace SpriterEngine;

class SpriterImageFile : public ImageFile
{
public:
    SpriterImageFile(const std::string& initialFilePath, point initialDefaultPivot) :
        ImageFile(initialFilePath, initialDefaultPivot) {
    }

    void renderSprite(UniversalObjectInterface *spriteInfo) override {
    }

    void setAtlasFile(AtlasFile *initialAtlasFile, atlasframedata initialAtlasFrameData) override {
		ImageFile::setAtlasFile(initialAtlasFile, initialAtlasFrameData);
    }

private:
};

class SpriterFileFactory : public FileFactory
{
public:
    SpriterFileFactory() {
    }

    ImageFile *newImageFile(const std::string& initialFilePath, point initialDefaultPivot, atlasdata atlasData) override {
        return new SpriterImageFile(initialFilePath, initialDefaultPivot);
    }

    //AtlasFile *newAtlasFile(const std::string &initialFilePath) override;

    //SoundFile *newSoundFile(const std::string &initialFilePath) override;

    //SpriterFileDocumentWrapper *newScmlDocumentWrapper() override;

	SpriterFileDocumentWrapper* newSconDocumentWrapper() override {
        return new JSONSpriterFileDocumentWrapper();
    }

private:
};

#endif
