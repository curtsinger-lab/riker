#pragma once

#include "util/serializer.hh"
#include "versions/Version.hh"

class ContentVersion : public Version {
 private:
  SERIALIZE(BASE(Version));
};