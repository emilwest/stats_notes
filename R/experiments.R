library(tidyverse)

as.numeric(iris$Species)
wilcox.test(iris$Sepal.Length ~ as.numeric(iris$Species) )
t.test(temp$AHI_diff ~ temp$Intervention)
boxplot(temp$AHI_diff~temp$Intervention)

p11 <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
# with common legend
p1 <- ggarrange(p11  ,
                p11 ,
                p11 ,
                ncol=1, nrow=3,  legend = "none"
)
p2 <- ggarrange( p11 ,  legend = "none")
p3 <- ggarrange(
  p11,
  p11 ,
  p11 , 
  ncol=1,nrow=3, common.legend = T, legend="right"
)
ggarrange(p1, p2, p3, ncol=3, common.legend = T, legend="right")
p11 <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, fill=Species)) + geom_point()
p11_c <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Sepal.Width)) + geom_point()

library(RColorBrewer)
num_categories <- 3
cls_size <- brewer.pal(num_categories, "Reds") 
p11 +  scale_color_manual("Hej", values = cls_size, drop=FALSE)
p11 + scale_color_brewer("Hej",palette = "Reds")


p11 + scale_color_viridis_d("Hej")
p11 + scale_color_grey()
p11 + scale_colour_discrete(palette = scales::hue_pal() )
scale_linetype_manual()

p11_c + scale_color_gradient(low = "white", high = "red")



fisher.test( matrix(c(2,10,20,3),nrow=2,ncol=2)  )
