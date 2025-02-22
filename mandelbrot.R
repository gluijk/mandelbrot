# Animation of the Mandelbrot Set
# www.overfitting.net
# https://www.overfitting.net/

library(tiff)
Gamma=2.2  # Gamma curve


ITERA=200  # Iterations
WIDTH=1920  # FUll HD
HEIGHT=1080

# MANDELBROT MATRIX CODE
xmin = -2.248148
xmax = 1.305556

ymin = -(xmax - xmin) * HEIGHT / WIDTH / 2
ymax = -ymin

x = seq(xmin, xmax, length.out = WIDTH) 
y = seq(ymin, ymax, length.out = HEIGHT) 
c = outer(x, y * 1i, FUN="+") 
z = array(0, c(length(x), length(y)))
Mandel = z

j=1
for (i in 1:ITERA) {
    indices = which(Mod(z) <= 2)
    z[indices] = z[indices] ^ 2 + c[indices]
    Mandel[indices] = Mandel[indices] + 1
    
    img=t(Mandel)
    img[img==i]=0  # set black to current estimated Mandelbrot set
    img=(img/max(img))^(1/Gamma)  # normalize output grayscale
    img=img[nrow(img):1,]  # flip rows
    img=replicate(3, img)  # add colour
    img[,,1:2]=(sin(pi*(img[,,1:2]-1)+pi/2)+1)/2  # add R and G contrast

    # Iterations not rendered linearly
    repeats=round((ITERA-i)^8/((ITERA-1)^(8-1))/3.33697002537963+1)
    for (k in 1:repeats) {
        print(paste0("Saving iteration ", i, " for ", repeats, " times..."))
        writeTIFF(img,
            paste0("mandelbrot", ifelse(j<10,'000', ifelse(j<100,'00',
            ifelse(j<1000,'0',''))), j, ".tif"),
            bits.per.sample=16, compression="LZW")
        j=j+1
    }
} 


# Build animation (1545 frames, 64.410s audio track, ~24fps)
# ffmpeg -loop 0 -framerate 23.98695855 -i mandelbrot%04d.tif /
# -i expanse.wav -t 64.410 -c:v libx264 -crf 23 -pix_fmt yuv420p mandelbrot.mp4