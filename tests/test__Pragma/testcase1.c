#define MACRO_PRAGMA \
    _Pragma("GCC warning \"test\"") \
    _Pragma("GCC whatever") \
    int x = 7; \
    _Pragma("GCC whatever") 

_Pragma("GCC warning \"hello\"") 
MACRO_PRAGMA
int main(void){
    
_Pragma("GCC warning \"hello\"") 
            MACRO_PRAGMA
    return x; 

}
