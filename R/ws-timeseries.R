#' Access to the 'timeseries' web service
#' 
#' @export
ws.timeseries <- function(net, sta, loc, cha, 
                          date.start, date.end=NULL, duration=NULL){
    # /query? (channel-options) (date-range-options) (filter-options) [plot-options] [audio-options] (output-options)
    # where:
    #   channel-options      ::  (net=<network> & sta=<station> & loc=<location> & cha=<channel>)
    #   date-range-options   ::  (starttime=<time>) & ([endtime=<time>] | [duration=<seconds>])
    #   filter-options       ::  [taper=WIDTH,TYPE] [envelope] [lpfilter=FREQ] [hpfilter=FREQ]
    #                            [bpfilter=HIFREQ-LOFREQ] [demean=<true|false>] 
    #                            [diff=<true|false>] [int=<true|false>] [scale=number|AUTO] 
    #                            [divscale=number] [correct=<true|false>] [freqlimits=F1-F2-F3-F4] 
    #                            [autolimits=lowerdBdown-upperdBdown] 
    #                            [units=<DEF|DIS|VEL|ACC>] [decimate=SAMPLERATE]
    #   plot-options         ::  [antialiasplot=<true|false>]
    #   audio-options        ::  [audiocompress=<true|false>] [audiosamplerate=<playback-rate-hz>]
    #   output-options       ::  (output=<miniseed|saca|sacbb|sacbl|plot|ascii|ascii1|ascii2|audio>)
    #   
    #   (..) required
    #   [..] optional   
}
