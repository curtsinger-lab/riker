#include "db.capnp.h"

void set_fingerprint(db::File::Builder file, bool use_checksum);
bool match_fingerprint(db::File::Reader file);
