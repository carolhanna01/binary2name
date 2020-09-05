#include "libGIFTQutest-plugin/include/CQtest-plugin.h"
#include "libMRML/include/CAccessorAdminCollection.h"
extern "C" char* libGIFTQutest-plugin_getClassName(){
  return "test-plugin";
}

extern "C" CQuery* libGIFTQutest-plugin_makeQuery(CAccessorAdminCollection& inAccessorAdminCollection,
					    CAlgorithm& inAlgorithm
					    ){
  return new CQtest-plugin(inAccessorAdminCollection,
		     inAlgorithm
		     );
}
