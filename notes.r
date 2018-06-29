# (1) 改了，看下面
# (2) 看不懂你的问题
# (3) 改了一下，但没到你别的algo
# (4) 需要你给数据的样子
# (5) 看不大懂你写的code，难以脑补，有空电话一下
# (6) 同 (4)
# (7) 是不是要判断，如果没装pa，给装一下？建议不用，如果后面编package可以直接加进去
# (8)改了，看下面
# (9) 过会儿写，单独写一个function，这个function调用你的GlobalAttributionMulti
# (10) 需要电话，看不懂你后面的code
# (11) 看不懂你的问题


# GlobalAttribution

1. 在d$fx_ret = d[,fx.var]的后面加上：

d = d %>% dplyr::arrange(country) %>% group_by(country)

然后wpc，wbc，rplc，rblc，等里面的group可以去掉	

可以直接写 GA$COUNTRY = c(wpc$country, "TOTAL")


2 .接着你在function里面有重新定义了fx.var，最后该一个名字

3. # check if the effects add up要改成if statement

if abs(active_return - active_return_2) > 0.0001 {
  warning("Effects do not add up."); stop()
}

# GlobalAttribution & GlobalAttributionMulti

1. 你用了很多dataFrame[,c("a","b","c"...)]这样的格式，
建议单独建一个比如 colSet1 = c("a","b","c"); colSet2 = c("a","c","d","e")
然后 dataFrame[,colSet1]; dataFrame[,colSet2]

2. GlobalAttributionMulti = function(d, startDate= NULL, endDate=NULL, 
                                     SmoothAlgo="Fudge",
                                     date.var="date", ............. ) {
  
  if (!(SmoothAlgo %in% c("Fudge", "你自己给个名字", "再给名字")) {
    warning("Invalid input of SmoothAlgo. Only aceept Fudge, 你自己给个名字, or 再给名字")
    stop()
  }
  
  if class(d[,date.var] != "Date") {
    warning("Invalid format of date column in the data table")
    stop()
  }
  
  
  # --------重命名-----------
  
  d$date = d[,date.var]; 
  d$... ................
  
  # --------------------------
  
  # 这一段我认为应该这样写，而不是给什么错误提示，那是GUI的写法
  if (!is.null(startDate)) {
    
    if (class(startDate) != "Date") {
      warning("Invalid input of startDate"); stop()
    } else {
      
      if (startDate < min(d$date) {
        warning("Entered startDate is ealier than the beginning date of the data")
      } else {
        d = d %>% dplyr::filter(date>=startDate)
      }
    }
    
  }
  
  if (!is.null(endDate)) {
    
    if (class(endDate) != "Date") {
      warning("Invalid input of endDate"); stop()
    } else {
      if (startDate > max(d$date) {
        warning("Entered endDate is later than the ending date of the data")
      } else {
        d = d %>% dplyr::filter(date<=endDate)
      }
    }
  }
  
  d = d %>% arrange(date, country)
  
  
  .......
  
  .......
  
  
  if (SmoothAlog == "Fudge") {
    怎样怎样
  } else if (SmmothAlog == "你自己给个名字") {
    怎样怎样
  } else if (SmoothAlog == "再给名字") {
    怎样怎样
  }
  
  
  .......
  
  # check if XXX add up 都要改成前面那样的
  if ( abs(AAA - BBB) > 0.00001) {
    warning("~~~~~~"); stop()
  }
  