package io
import java.awt.Image
import java.awt.datatransfer._

class ImageClipboard(image: Image) extends Transferable {
  
  def getTransferData(flavor: DataFlavor): Object = {
    if (!DataFlavor.imageFlavor.equals(flavor))
    {
      throw new UnsupportedFlavorException(flavor);
    }
    return image;
    
  } 
  
  def getTransferDataFlavors(): Array[DataFlavor] = {
    return Array(DataFlavor.imageFlavor)
  }
  
  def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
    return DataFlavor.imageFlavor.equals(flavor)
  } 
}