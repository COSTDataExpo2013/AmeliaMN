#ifndef dplyr_Name_H
#define dplyr_Name_H

namespace dplyr {
    
    class Name {
    public:
    
        Name( SEXP s ) : 
            original( TYPEOF(s) == CHARSXP ? s : PRINTNAME(s) ), 
            utf8(StringUtf8(original)){}
        
        Name( const CharacterVector::Proxy& proxy ) : 
            original( proxy ), 
            utf8(StringUtf8(original)){}
        
        
        inline bool operator==( const Name& other ) const {
            return other.utf8 == utf8 ;    
        }
             
        inline SEXP get() const {
            return original ;
        }
        
        inline size_t hash() const {
            return boost::hash<SEXP>()(utf8) ;    
        }
        
        inline operator SEXP() const {
            return original ;    
        }
        
    private:
        
        SEXP original ;
        SEXP utf8 ;
    } ;
    
    inline std::size_t hash_value(const Name& name){
        return name.hash() ;
    }
    
}

#if defined(_WIN32)
namespace std {
    namespace tr1 {
        
        template <>
        struct hash< dplyr::Name > {
            std::size_t operator()( const dplyr::Name & c ) const { 
                return dplyr::hash_value( c ); 
            }
        };
        
    }
}
#endif

#endif
