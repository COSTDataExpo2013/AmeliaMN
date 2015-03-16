#ifndef dplyr_Result_Reducer_H
#define dplyr_Result_Reducer_H

namespace dplyr {
    
    template <typename OUT, int INPUT_RTYPE> class Reducer : public Processor< Rcpp::traits::r_sexptype_traits<OUT>::rtype, Reducer<OUT,INPUT_RTYPE> > {
    public:
        const static int OUTPUT = Rcpp::traits::r_sexptype_traits<OUT>::rtype ;
        Reducer(Rcpp::Function fun_, SEXP data_ ) :
            call(fun_, R_NilValue), 
            proxy(call, 1), 
            data(data_)
        {}
        
        Reducer( const Reducer& other ) : 
            call(other.call),
            proxy(call, 1), 
            data(other.data)
        {}
        
        OUT process_chunk( const SlicingIndex& indices ){
            proxy = wrap_subset<INPUT_RTYPE>( data, indices ) ;
            return Rcpp::as<OUT>( call.fast_eval() ) ;    
        }
        
        // used by DelayedReducer
        SEXP delayed_process(const Rcpp::GroupedDataFrame& gdf, SEXP first_result) {
            Rcpp::Shelter<SEXP> __ ;
            int n = gdf.ngroups() ; 
            SEXP res = __( Rf_allocVector( OUTPUT, n) ) ;
            OUT* ptr = Rcpp::internal::r_vector_start<OUTPUT>(res) ;
            ptr[0] = Rcpp::as<OUT>( first_result );
            GroupedDataFrame::group_iterator git = gdf.group_begin() ;
            for( int i=1; i<n; i++,++git)
                ptr[i] = process_chunk(*git) ;
            return res ;        
        }
        
    
    private:
        Rcpp::Language call ;
        Rcpp::Language::Proxy proxy ;
        SEXP data ; 
        
    } ;
    
    
    template <int INPUT_RTYPE> class Reducer<Rcpp::String,INPUT_RTYPE> : 
        public Processor<STRSXP, Reducer<Rcpp::String,INPUT_RTYPE> > {
    public:
        Reducer(Rcpp::Function fun_, SEXP data_ ) :
            call(fun_, R_NilValue), 
            proxy(call, 1), 
            data(data_)
        {}
        Reducer(Rcpp::Language call_, SEXP data_ ) : call(call_), proxy(call,1), data(data_){}
        
        Reducer( const Reducer& other ) : 
            call(other.call),
            proxy(call, 1), 
            data(other.data)
        {}
        
        SEXP process_chunk( const SlicingIndex& indices ){
            proxy = wrap_subset<INPUT_RTYPE>( data, indices ) ;
            return STRING_ELT( call.fast_eval(), 0 ) ;    
        }
        
        // used by DelayedReducer
        SEXP delayed_process(const Rcpp::GroupedDataFrame& gdf, SEXP first_result) {
            int n = gdf.ngroups() ; 
            Rcpp::Shield<SEXP> res( Rf_allocVector( STRSXP, n) ) ;
            SET_STRING_ELT( res, 0, STRING_ELT( first_result, 0 ) );
            GroupedDataFrame::group_iterator git = gdf.group_begin() ;
            for( int i=1; i<n; i++,++git)
                SET_STRING_ELT( res, i, process_chunk(*git) );
            return res ;        
        }
        
    
    private:
        Rcpp::Language call ;
        Rcpp::Language::Proxy proxy ;
        SEXP data ; 
        
    } ;
    
    
}

#endif
