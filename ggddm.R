ggddm <- function(ggddm) {

  require('ggplot2')
  require('extrafont')
  loadfonts(device = "postscript")
  
  if (exists("ggddm_df")){
    label1<-ggddm_df$label1[1]
    a1<-mean(ggddm_df$a1)
    z1<-mean(ggddm_df$z1)
    v1<-mean(ggddm_df$v1)
    condition1<-ggddm_df$condition1[1]
    color1<-ggddm_df$color1[1]
      
    label2<-ggddm_df$label2[1]
    a2<-mean(ggddm_df$a2)
    z2<-mean(ggddm_df$z2)
    v2<-mean(ggddm_df$v2)
    condition2<-ggddm_df$condition2[1]
    color2<-ggddm_df$color2[1]
    
    font<-ggddm_df$font
  } else {
    
  
  
  if (exists("label1")){
    label1 <- label1
   } else {
      label1 <- as.character(readline(prompt="Enter label for upper boundary (leave blank for default): "))
  }
  #label1 <- as.character(readline(prompt="Enter label for upper boundary (leave blank for default): "))
  label1 <- ifelse(nchar(label1)==0,1,label1)
  
  if (exists("label2")){
    label2 <- label2
  } else {
  label2 <- as.character(readline(prompt="Enter label for lower boundary (leave blank for default): "))
  }  
  #label2 <- as.character(readline(prompt="Enter label for lower boundary (leave blank for default): "))
  label2 <- ifelse(nchar(label2)==0,0,label2)

  if (exists("a1")){
    a1 <- a1
  } else {
  a1 <- as.numeric(readline(prompt="Enter a-Parameter of first condition (typical range: 0.5 < a < 2): "))
  }
  a1 <- ifelse(nchar(a1)==0,1,a1)
  
  if (exists("z1")){
    z1 <- z1
  } else {
  z1 <- as.numeric(readline(prompt="Enter z-Parameter of first condition (typical range: 0.3 < z < 0.7): "))
  }
  z1 <- ifelse(nchar(z1)==0,0.5,z1)
  
  if (exists("v1")){
    v1 <- v1
  } else {
  v1 <- as.numeric(readline(prompt="Enter v-Parameter of first condition (typical range: -5 < v < 5): "))
  }
  v1 <- ifelse(nchar(v1)==0,1,v1)
  
  if (exists("condition1")){
    condition1 <- condition1
  } else {
  condition1 <- as.character(readline(prompt="Enter label for first condition: "))
  }
  condition1 <- ifelse(nchar(condition1)==0,"condition 1",condition1)
  
  if (exists("color1")){
    color1 <- color1
  } else {
  color1 <- as.character(readline(prompt="Enter color for first condition (leave blank for default): "))
  }
  color1 <- ifelse(nchar(color1)==0,"#0451FC",color1)

  if (exists("a2")){
    a2 <- a2
  } else {
  a2 <- as.numeric(readline(prompt="Enter a-Parameter of second condition (typical range: 0.5 < a < 2): "))
  }
  a2 <- ifelse(nchar(a2)==0,1.1,a2)
  
  if (exists("z2")){
    z2 <- z2
  } else {
  z2 <- as.numeric(readline(prompt="Enter z-Parameter of second condition (typical range: 0.3 < z < 0.7): "))
  }
  z2 <- ifelse(nchar(z2)==0,0.6,z2)
  
  
  if (exists("v2")){
    v2 <- v2
  } else {
  v2 <- as.numeric(readline(prompt="Enter v-Parameter of second condition (typical range: -5 < v < 5): "))
  }
  
  if (exists("condition2")){
    condition2 <- condition2
  } else {
  condition2 <- as.character(readline(prompt="Enter label for second condition: "))
  }
  condition2 <- ifelse(nchar(condition2)==0,"condition 2",condition2)
  
  if (exists("color2")){
    color2 <- color2
  } else {
  color2 <- as.character(readline(prompt="Enter color for second condition (leave blank for default): "))
  }
  color2 <- ifelse(nchar(color2)==0,"#ff7d30",color2)
  
  
  font <- as.character(readline(prompt="Enter font name (leave blank for default): "))
  
  
  }

  x1 = 0   # constant starting process (0 = without non decision time)
  z0 = 0.5 # constant
  h1 = 0   # lower boundary
  h3 = 1   # upper boundary

xy <- data.frame(
  
  label1<-label1,
  label2<-label2,
  
  x1 <- x1,

  v1 <- v1,
  v2 <- v2,
  
  a <- ifelse(a1>a2,a1,a2),
  a1 <- a1/a,
  a2 <- a2/a,
  
  z <- ((z1+z2)/2),
  z1 <- z1*a1,
  z2 <- z2*a2,

  h2 <- z0,
  
  a_lab_up <- ifelse(a1>a2,.05,-.05),
  a_lab_down <- ifelse(a1>a2,-.05,.05),

  xend_biasz0_v1 <- (exp(-(2*v1*a1))-(exp(-(2*v1*z1))))/(exp(-(2*v1*a1))-1),
  xend_biasz0_v2 <- (exp(-(2*v2*a2))-(exp(-(2*v2*z2))))/(exp(-(2*v2*a2))-1)
)

diff_all <- ggplot(xy, aes(x,y)) +
  xlim(-0.05, 1.05)+ # plot position x scale
  ylim(-0.05, 1.05)+ # plot position y scale
  geom_segment(aes(x1, z1 , xend = xend_biasz0_v1, yend = a1, colour = "v1"), size = 1.5, data = xy, arrow = arrow(length = unit(0.5, "cm")), show.legend = FALSE)+ #both v's
  geom_segment(aes(x1, z2 , xend = xend_biasz0_v2, yend = a2, colour = "v2"), size = 1.5, data = xy, arrow = arrow(length = unit(0.5, "cm")), show.legend = FALSE)+
  geom_segment(aes(x1 , h1 ,    xend = 1, yend = 0),   size = 1.2, data = xy,  show.legend = FALSE)+ #lower boundary
  geom_segment(aes(x1 , a1 ,    xend = 1, yend = a1, colour = "v1"),   size = 1.2, data = xy,  show.legend = FALSE)+ #upper boundary a1
  geom_segment(aes(x1 , a2 ,    xend = 1, yend = a2, colour = "v2"),   size = 1.2, data = xy,  show.legend = FALSE)+ #upper boundary a2
  theme(
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks   = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank()) +
  geom_text(x= 1, y=1.05,  label=label1, size = 8, family = font, hjust = 1) +
  geom_text(x= 1, y=-0.05, label=label2,  size = 8, family = font, hjust = 1) +
  geom_text(x=-0.05, y=0,    label="0", hjust = 0, size = 8, family = font) +
  geom_text(x=-0.05, y=z, label="z", hjust = 0, size = 8, family = font) +
  geom_text(x=-0.05, y=1.01, label="a", hjust = 0, size = 8, family = font) +
  geom_text(x=0.9, y=0.45, label="time", hjust = 0, size = 8, family = font) +
  geom_text(x=xy$xend_biasz0_v1, y=a1+a_lab_up, label=paste("v",condition1), hjust = 0, size = 6, color = color1, family = font) +
  geom_text(x=xy$xend_biasz0_v2, y=a2+a_lab_down, label=paste("v",condition2), hjust = 0, size = 6, color = color2, family = font) +
  geom_segment(aes(x1 , h2 ,    xend = 1, yend = 0.5), size = 1,   data = xy,  show.legend = FALSE, linetype = "dashed", arrow = arrow(length = unit(0.5, "cm")))+
  geom_segment(aes(x=0 , y=0 ,    xend = 0, yend = 1),   size = 1.2, data = xy,  show.legend = FALSE) +# vertikale Linie
  scale_color_manual(values=c(color1,color2))
return(diff_all)
}
