package com.ugos.awt;

import java.awt.event.*;
import java.awt.*;

public class TextMessageDialog extends Dialog implements WindowListener
{
    private Button    m_btnOk;
    private TextArea  m_TextMsg;
    
    //public static final int CHAR_WEIGHT = 7;
    private static int HEIGHT = 200;
    private static int WIDTH = 340;
    
    public TextMessageDialog(Frame parent, String strTitle, String strMessage)
    {
        super(parent, strTitle, true);
        
        addWindowListener(this);
        
         // windows dimensions
        Dimension ds = Toolkit.getDefaultToolkit().getScreenSize();
        
        int nWidth =  WIDTH > ds.width ? ds.width : WIDTH;
        int nHeight = HEIGHT > ds.height ? ds.height : HEIGHT;
                        
        int nTop  = (ds.height - nHeight) / 2;
        int nLeft = (ds.width  - nWidth)  / 2;
        //setSize(nWidth, nHeight);
        setBounds(nLeft, nTop, nWidth, nHeight);
        
        // font size
        Font fnt = new Font ("Arial", Font.PLAIN, 10);
        
        // Crea TextArea da inserire nella finestra
        m_TextMsg = new TextArea(strMessage, 7, 1, TextArea.SCROLLBARS_VERTICAL_ONLY);
        m_btnOk   = new Button("  Ok  ");
        
        m_TextMsg.setEditable(false);
        m_TextMsg.setFont(fnt);
        m_btnOk.setFont(fnt);
                        
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        setLayout(gbl);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx   = 2;
        gbc.fill      = GridBagConstraints.HORIZONTAL;
        gbc.insets    = new Insets(5, 5, 5, 5);
        gbl.setConstraints(m_TextMsg, gbc);
        add(m_TextMsg);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx   = 1;
        gbc.anchor    = GridBagConstraints.CENTER;
        gbc.fill      = GridBagConstraints.NONE;
        gbc.ipadx     = 20;
        gbl.setConstraints(m_btnOk, gbc);
        add(m_btnOk);
                                               
//        setTextFont(new Font("Arial", Font.PLAIN, 11));
    }
    
    public void setTextFont(Font f)
    {
        m_TextMsg.setFont(f);
        m_btnOk.setFont(f);
//
//        int nChar = getTitle().length();
//        //int nWidth = nChar * XDialog.CHAR_WEIGHT;
//        int nWidth = nChar * (Toolkit.getDefaultToolkit().getFontMetrics(f).charWidth('A'));
//        nWidth = nWidth  > WIDTH ? nWidth : WIDTH;
//
//        // Centra la dialog
//        Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
//
//        int nTop  = (d.height - HEIGHT) / 2;
//        int nLeft = (d.width  - nWidth)  / 2;
//        //int nLeft = (d.width  - WIDTH)  / 2;
//
//        setBounds(nLeft, nTop, nWidth, HEIGHT);
    }

    public boolean action(Event evt, Object arg)
    {
        if(arg.equals(m_btnOk.getLabel()))
        {
            onOk();
            return true;
        }

        return super.action(evt, arg);
    }
    
    protected void onOk()
    {
        windowClosing(null);
    }
    
    public void windowActivated(WindowEvent e)
    {
        m_btnOk.requestFocus();
    }

    public void windowClosing(WindowEvent e)
    {
        setVisible(false);
        dispose();
    }

    public void windowClosed(WindowEvent e){}
    public void windowDeactivated(WindowEvent e){}
    public void windowDeiconified(WindowEvent e){}
    public void windowIconified(WindowEvent e){}
    public void windowOpened(WindowEvent e){}
}
