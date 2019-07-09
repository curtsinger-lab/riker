#include "db.capnp.h"

void set_fingerprint(db::File::Builder file);
bool match_fingerprint(db::File::Reader file);
