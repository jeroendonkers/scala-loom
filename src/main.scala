import loom._
import io._
import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Toolkit
import event._
import scala.collection.immutable.SortedMap
import scala.io.Source
import java.io._
import javax.imageio.ImageIO

class Canvas extends Panel {

  private[this] var compiled = false
  private[this] var img: BufferedImage = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
  private[this] var weaving: List[List[Boolean]] = List()
  private[this] var flipped = false
  private[this] var zoom = 5
  
  val colors: Array[Color] = Array(Color.WHITE,Color.BLACK,Color.RED,Color.GREEN,
      Color.GRAY, Color.GRAY, Color.GRAY, Color.GRAY, Color.GRAY, Color.GRAY)
  val coloractive: Array[Boolean] = colors.map(c => false)    
  
  override def paint(g: Graphics2D) {
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, size.width, size.height)      
    if (compiled) {
        g.drawImage(img,0,0,null)
    }   
  }
  
  
  def clear() {
    compiled=false
    List.range(0,colors.size).foreach(k => coloractive(k)=false)
    repaint()
  }
  
  def makeimage() {
     img = Cloth.makeImage(zoom,weaving,Pattern("warpcolors"),Pattern("weftcolors"),colors.toList, flipped)
     preferredSize  = new Dimension(img.getWidth(),img.getHeight())
     revalidate()
  }
  
  def compile(s: String) {
     PatternParser.eval(s) match {
       case None => {
           val ers: String = PatternParser.errors.mkString("\n")
           Dialog.showMessage(null, ""+ers, "Parsing error", Dialog.Message.Error ,null)
           compiled=false
           repaint()
           return
         }
       case Some(_) => { 
         val loom = new Loom(Pattern("tieup"), Pattern("threading"))
         weaving = loom.weave(Pattern("pedalling"))
         List.range(0,colors.size).foreach(k => {
           
             if (Pattern.isdefined("color"+(k+1))) {
              val rgb = Pattern("color"+(k+1)).list
              val col = new Color(rgb(0),rgb(1),rgb(2))
              setColor(k,col)
              coloractive(k) = true
             } else {
              coloractive(k) = false
             }
         })
        makeimage()     
        maximumSize = new Dimension(img.getWidth, img.getHeight)
        compiled=true
        repaint()
     }}    
  }
  
  def setColor(i: Int, c: Color) {
    colors(i) = c
    if (compiled) {
      makeimage()
      repaint()
    }
  }

  def isCompiled: Boolean = { compiled }
  
  def copyToClipboard() {
    val imgSel = new ImageClipboard(img);
    Toolkit.getDefaultToolkit().getSystemClipboard().setContents(imgSel, null);
  }
  
  def saveImage(f: File) {
     ImageIO.write(img,"PNG",f)
  }
  
  def flip() {
    flipped =  !flipped
    if (compiled) {
      makeimage()
      repaint()
    }
  }
  
  def zoomout() {
    if (compiled & (zoom>1)) {
      zoom -= 1
      makeimage()
      repaint()
    }
  }
  
  def zoomin() {
    if (compiled) {
      zoom += 1
      makeimage()
      repaint()
    }
  }  
  
}

class PngFilter extends javax.swing.filechooser.FileFilter  {
  def accept(file: File): Boolean  = {
        val filename = file.getName();
       return filename.endsWith(".png");
    }
    def getDescription(): String  = {
      return "*.png";
    }
}



class ColorButton(val color: Int) extends Button 
class ColorMenuItem(val color: Int, action: Action) extends MenuItem(action)

object main extends SimpleSwingApplication {
  var loaded = false
  var changed = false
  var file: File = null
  var actdir = new File("patterns/.")
  val  pngfilter = new javax.swing.filechooser.FileNameExtensionFilter("PNG FILES", "png", "png image");  
  
  def top = new MainFrame {
    var filename = "None"
      
    def makeTitle() = {
      title = "Loom: "+filename + (if (changed) "*"else "")  
    }
    
    makeTitle()
      
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)  
    
    def intKey(k: Int): Key.Value = {
        k match {
                case 0 => Key.Key1
                case 1 => Key.Key2
                case 2 => Key.Key3
                case 3 => Key.Key4
                case 4 => Key.Key5
                case 5 => Key.Key6
                case 6 => Key.Key7
                case 7 => Key.Key8
                case 8 => Key.Key9
                case 9 => Key.Key0
        }        
    }
      
    val canvas = new Canvas {
          preferredSize = new Dimension(500, 250)
    }
    val scrollpane = new ScrollPane
    scrollpane.viewportView = canvas
    
    val textArea = new EditorPane  {
          preferredSize = new Dimension(500, 250)
          peer.setAutoscrolls(true)
    }
    val textpane = new ScrollPane 
    textpane.viewportView = textArea
    
    val weavebutton = new Button {
      text = "Weave"
      borderPainted = true
      enabled = true
      tooltip = "Click to run script"
      mnemonic = Key.W
    }
    val flipbutton = new ToggleButton {
      text = "Flip"
      borderPainted = true
      enabled = true
      tooltip = "Flip Cloth"
      mnemonic = Key.P
    }
    val zoominbutton = new Button {
      text = "++"
      borderPainted = true
      enabled = true
      tooltip = "Zoom in"
    }
    val zoomoutbutton = new Button {
      text = "--"
      borderPainted = true
      enabled = true
      tooltip = "Zoom out"
    }
    
    val colorbuttons = List.range(0,canvas.colors.size).map(k => 
        new ColorButton(k) {
        	text = "C" + (k+1)  
        	borderPainted = true
        	enabled = true
        	tooltip = "Change color "+(k+1)
        	background = canvas.colors(k)
        	mnemonic = intKey(k)
        	visible = false
        }
     )
     
  val colormenus = List.range(0,canvas.colors.size).map(k =>
    new ColorMenuItem(k,Action("color "+(k+1)) {
          ColorChooser.showDialog(canvas, "Select color "+(k+1), canvas.colors(k)).foreach(
                  c => { canvas.setColor(k,c); colorbuttons(k).background=c}  
        ) }) { mnemonic = intKey(k)
               visible = false }
  )
  
    def updateColorButtons = {
      colorbuttons.foreach(  b => { b.background = canvas.colors(b.color)
          b.visible = canvas.coloractive(b.color)}
       )
      colormenus.foreach(m => {
          m.visible = canvas.coloractive(m.color)}
       )
       
    }
     
    val buttonbar = new FlowPanel {
      contents+=weavebutton
      contents+=flipbutton
      contents+=zoominbutton
      contents+=zoomoutbutton
      colorbuttons.foreach(b => contents+=b)
    }
    
    val imagepanel =  new BorderPanel {
      layout(scrollpane) = Center
      layout(buttonbar) = South
    } 
    
    contents = new SplitPane() {
       leftComponent  = textpane
       rightComponent =  imagepanel
       dividerLocation = 250
    }  
  
     peer.setLocationRelativeTo(null)
    
    def saveFile() = {
         val w = new BufferedWriter(new FileWriter(file.getAbsoluteFile()))
         w.write(textArea.text)
         w.close()
         changed=false
         filename = file.getName()
         makeTitle()
    }
    
    def saveFileAs() {
         val chooser = new FileChooser(file)
         chooser.title = "Save as"
         val result = chooser.showSaveDialog(null)
         if (result == FileChooser.Result.Approve) {
               file = chooser.selectedFile
               val w = new BufferedWriter(new FileWriter(file.getAbsoluteFile()))
               w.write(textArea.text)
               w.close()
         }
         changed=false
         filename = file.getName()
         makeTitle()
    }
    
    def checkSave(): Boolean = {
        val res = Dialog.showConfirmation(contents.head, 
				      "Do you want to save changes?", 
				      optionType=Dialog.Options.YesNoCancel,
				      title=title)
         if (res == Dialog.Result.Ok) {
           if (loaded) saveFile() else saveFileAs()
         }
        return (res != Dialog.Result.Cancel) 
    }
    
    def loadFile() {
         if (changed) { if (!checkSave()) return }
         val chooser = new FileChooser(actdir)
         chooser.title = "Select file"
         val result = chooser.showOpenDialog(null)
         if (result == FileChooser.Result.Approve) {
             actdir = new File(chooser.selectedFile.getAbsolutePath()) 
             file = chooser.selectedFile
             loaded = true
             textArea.text = Source.fromFile(file).mkString
             changed=false
             filename = file.getName()
             makeTitle()
             canvas.clear()
             updateColorButtons
         }    
    }
    
    def newFile() {
       if (changed) { if (!checkSave()) return }
       loaded = false
       textArea.text = ""  
       changed=false
       filename = "None"
       makeTitle()         
       canvas.clear()
       updateColorButtons
    }
    
    def savePng() = {
         val chooser = new FileChooser(new File("."))
         chooser.title = "Save image as"
         chooser.fileFilter = pngfilter
         val result = chooser.showSaveDialog(null)
         if (result == FileChooser.Result.Approve) {
               if (chooser.selectedFile.getAbsolutePath().endsWith(".png")) 
                  canvas.saveImage(chooser.selectedFile)
               else   
                  canvas.saveImage(new File(chooser.selectedFile.getAbsolutePath()+".png"))
         }     
    }
    
    menuBar = new MenuBar {
      contents += new Menu("Color") {
        mnemonic = Key.C
        colormenus.foreach(m => contents += m)  
      } 
          
            
      contents += new Menu("Image") {
            mnemonic = Key.I
            contents += new MenuItem(Action("Save image") {
              if (canvas.isCompiled) savePng()
            }) {
              mnemonic = Key.S
            }
            contents += new MenuItem(Action("Copy to clipboard") {
              if (canvas.isCompiled) canvas.copyToClipboard()
            }) {
              mnemonic = Key.C
            }

      }
      
       contents += new Menu("File") {
            mnemonic = Key.F 
            contents += new MenuItem(Action("Load file") {
              loadFile()
            }) {
              mnemonic = Key.L
            }
            contents += new MenuItem(Action("Save file") {
              if (loaded) saveFile()
            }) {
              mnemonic = Key.S
            }
            contents += new MenuItem(Action("Save file As...") {
              if (loaded) saveFileAs()
            }) {
              mnemonic = Key.A
            }
            contents += new MenuItem(Action("New") {
              newFile()
            }) {
              mnemonic = Key.N
            }
            contents += new Separator()
            contents += new MenuItem(Action("Exit") {
              closeOperation() 
            }) {
              mnemonic = Key.E
            }
       }   
     } // menubar
    
      listenTo(weavebutton, flipbutton, zoominbutton, zoomoutbutton, textArea)
      colorbuttons.foreach(listenTo(_))
      reactions += {
      case ButtonClicked(component) => {
        if (component == weavebutton) { 
          canvas.compile(textArea.text)
          updateColorButtons
        }
        if (component == flipbutton) canvas.flip()
        if (component == zoominbutton) canvas.zoomin()
        if (component == zoomoutbutton) canvas.zoomout()
        colorbuttons.foreach(b => 
          if (component == b ) {
              ColorChooser.showDialog(b, "Select color "+(b.color+1), canvas.colors(b.color)).foreach(
                  c => { canvas.setColor(b.color,c); b.background = c}  
               )
            
          })
      }   
        
      } 
       reactions += {
         case ValueChanged(component) => {
           changed=true
           makeTitle()
           //textArea.revalidate
         }
       }
        
     override def closeOperation() { if (changed) { if (checkSave()) sys.exit(0) } else sys.exit(0) }
       
       
  } // top
}

