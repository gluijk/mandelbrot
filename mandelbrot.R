# Animation of the Mandelbrot Set
# www.overfitting.net
# https://www.overfitting.net/


library(tiff)
Gamma=2.2  # Gamma curve


NewBitmap = function(dimx, dimy, val=0) {
    # Crea bitmap de dimensiones dimx y dimy
    return(array(val,c(dimx,dimy)))
}

# Por Carlos Gil Bellosta
indices.drawline = function(x0, y0, x1, y1) {
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
    if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
        m = (y1 - y0) / (x1 - x0)
        cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
    } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
    # Llamada traspuesta recursiva y traspuesta
}

DrawLine = function(img, x0, y0, x1, y1, inc=TRUE, val=1) {
    # Dibuja recta desde (x0,y0)-(x1,y1)
    # Por defecto método no destructivo y con valor=1
    indices=indices.drawline(x0, y0, x1, y1)
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawRect = function(img, x0, y0, x1, y1, inc=TRUE, val=1, fill=FALSE) {
    # Dibuja rectángulo (x0,y0)-(x1,y1)
    # Por defecto método no destructivo, con valor=1 y sin relleno
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (fill) {
        if (inc) img[x0:x1,y0:y1]=img[x0:x1,y0:y1]+val
        else img[x0:x1,y0:y1]=val
        
        return(img)
    } else {
        indices=which( ( (row(img)==x0         | row(img)==x1        ) &
                             (col(img)>=min(y0,y1) & col(img)<=max(y0,y1)) ) |
                           ( (col(img)==y0         | col(img)==y1        ) &
                                 (row(img)>=min(x0,x1) & row(img)<=max(x0,x1)) ) )
        if (inc) img[indices]=img[indices]+val
        else img[indices]=val
        
        return(img)
    }
}

DibujarNumero = function(img, x0, y0, inc=FALSE, val=1, fill=FALSE,
                         num, width, height) {
    # Dibuja cifra 0-9 en (x0,y0)
    # Por defecto método no destructivo y con valor=1
    
    if (num=='0') { 
        img=DrawRect(img, x0, y0, x0+width, y0-height, inc, val, fill)
    } else if (num=='1') {
        img=DrawLine(img, x0+width/2, y0, x0+width/2, y0-height, inc, val)
    } else if (num=='2') {
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0+width, y0-height/2, x0, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0, y0-height, inc, val)
        img=DrawLine(img, x0, y0-height, x0+width, y0-height, inc, val)
    } else if (num=='3') {
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height, x0+width, y0-height, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height, inc, val)
    } else if (num=='4') {
        img=DrawLine(img, x0, y0, x0, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height, inc, val)
    } else if (num=='5') {
        img=DrawLine(img, x0+width, y0, x0, y0, inc, val)
        img=DrawLine(img, x0, y0, x0, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0+width, y0-height/2, x0+width, y0-height, inc, val)
        img=DrawLine(img, x0+width, y0-height, x0, y0-height, inc, val)
    } else if (num=='6') {
        img=DrawRect(img, x0, y0-height/2, x0+width, y0-height, inc, val, fill)
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0, y0, x0, y0-height/2, inc, val)
    } else if (num=='7') {
        img=DrawLine(img, x0, y0, x0+width, y0, inc, val)
        img=DrawLine(img, x0+width, y0, x0+width, y0-height, inc, val)
    } else if (num=='8') {
        img=DrawRect(img, x0, y0, x0+width, y0-height/2, inc, val, fill)
        img=DrawRect(img, x0, y0-height/2, x0+width, y0-height, inc, val, fill)
    } else if (num=='9') {
        img=DrawRect(img, x0, y0, x0+width, y0-height/2, inc, val, fill)
        img=DrawLine(img, x0+width, y0-height/2, x0+width, y0-height, inc, val)
        img=DrawLine(img, x0, y0-height, x0+width, y0-height, inc, val)
    } else if (num=='-') {
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
    } else if (num=='m') {
        img=DrawLine(img, x0, y0-height/2, x0+width, y0-height/2, inc, val)
        img=DrawLine(img, x0, y0-height/2, x0, y0-height, inc, val)
        img=DrawLine(img, x0+width/2, y0-height/2, x0+width/2, y0-height, inc, val)
        img=DrawLine(img, x0+width, y0-height/2, x0+width, y0-height, inc, val)
    } else {
        return(img)  # Cifra inválida
    }
    
    return(img)
}


#########################################


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
    img[img==i]=0  # set black current estimated Mandelbrot set
    MAXIMG=max(img)
    img=(img/ifelse(MAXIMG==0,1,MAXIMG))^(1/Gamma)  # normalize output grayscale
    img=img[nrow(img):1,]  # flip rows
    
    # Draw iteration number
    TXT=as.character(i)
    LONG=nchar(TXT)
    label=NewBitmap(119, 63)
    for (k in 1:LONG) {
        num=substring(TXT, k, k)
        label=DibujarNumero(label, 2+k*44-44, 62, num=num, width=28, height=60)
    }
    label=t(label[,ncol(label):1])
    pos=which(label!=0)
    VAL=mean(img[1000:1062,1780:1898])
    VAL=ifelse(VAL>0.5, 0, 1)
    img[1000:1062,1780:1898][pos]=VAL
    img[1001:1063,1781:1899][pos]=VAL

    # Colour frames
    img=replicate(3, img)
    img[,,1:2]=(sin(pi*(img[,,1:2]-1)+pi/2)+1)/2  # add R and G contrast

    # Iterations not rendered linearly
    repeatframe=round((ITERA-i)^8/((ITERA-1)^(8-1))/3.33697002537963+1)
    print(paste0("Saving iteration ", i, "/", ITERA,
                 " for ", repeatframe, " times..."))
    for (k in 1:repeatframe) {
        name=paste0("mandelbrot", ifelse(j<10,'000', ifelse(j<100,'00',
                    ifelse(j<1000,'0',''))), j, ".tif")
        writeTIFF(img, name, bits.per.sample=16, compression="LZW")
        j=j+1
    }
} 


# Build animation (1545 frames, 64.410s audio track, ~24fps)
# ffmpeg -loop 0 -framerate 23.98695855 -i mandelbrot%04d.tif /
# -i expanse.wav -t 64.410 -c:v libx264 -crf 23 -pix_fmt yuv420p mandelbrot.mp4









