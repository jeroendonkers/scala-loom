package loom
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.geom.Rectangle2D
import java.awt.Paint

// turning a weaving into a picture
//
// parameters:
// - scale: size in pixels of each crossing
// - weaving: output of loom.weave
// - warpcolor, weftcolor: pattern containing color numbers
//   (both patterns are repeated if needed to fit weaving size)
// - colors: a list of awt.colors
// - flipped: show heddles pulled up or heddles pulled down: front or back side
// output: a bufferedImage

object Cloth {
  
  def makeImage(scale: Int, weaving: List[List[Boolean]], warpcolor: Pattern, weftcolor: Pattern, colors: List[Color], flipped: Boolean): BufferedImage = {
    
    val nweft = weaving.size
    if (nweft<=0) { return null } 
    val nwarp = weaving(0).size
    if (nwarp<=0) { return null } 
    val warpcolors = (warpcolor * (1+(nwarp / warpcolor.size))).list
    val weftcolors = (weftcolor * (1+(nweft / weftcolor.size))).list

    val img = new BufferedImage(nwarp*scale, nweft*scale, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, img.getWidth, img.getHeight)
    
    def pixel(x: Int, y: Int, c: Color, vertical: Boolean) {
      g.setColor(c)
      g.fill(new Rectangle2D.Double(x*scale, y*scale, scale, scale))
      val d = c.darker
      if (scale>2) {
        g.setColor(d)
        if (vertical) {
            g.drawLine(x*scale, y*scale, x*scale, (y+1)*scale)
            g.drawLine((x+1)*scale-1, y*scale, (x+1)*scale-1, (y+1)*scale)
        } else { 
            g.drawLine(x*scale, y*scale, (x+1)*scale, y*scale)
            g.drawLine(x*scale, (y+1)*scale-1, (x+1)*scale, (y+1)*scale-1)
        }   
       } 
    }
    
    List.range(0,nweft).foreach(e => List.range(0,nwarp).foreach(a =>
       if (weaving(e)(a) ^ flipped) pixel(a,e, colors(warpcolors(a)-1),true)
       else pixel(a,e, colors(weftcolors(e)-1),false)
    ))
    
    
    g.dispose()
    
    img
  }

  
}