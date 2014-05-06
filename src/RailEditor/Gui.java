package RailEditor;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.layout.FillLayout;
import java.awt.Toolkit;

public class Gui {

	protected Shell shell;
	private StyledText styledText;

	/**
	 * Launch the application.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			Gui window = new Gui();
			window.open();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Open the window.
	 */
	public void open() {
		Display display = Display.getDefault();
		createContents();
		shell.open();
		shell.layout();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
	}

	/**
	 * Create contents of the window.
	 */
	protected void createContents() {
		shell = new Shell();
		shell.setSize(450, 300);
		shell.setBounds(
				Toolkit.getDefaultToolkit().getScreenSize().width / 2 - 450 / 2,
				Toolkit.getDefaultToolkit().getScreenSize().height / 2 - 300 / 2,
				450, 300);
		shell.setText("SWT Application");
		shell.setLayout(new FillLayout(SWT.HORIZONTAL));

		styledText = new StyledText(shell, SWT.BORDER | SWT.H_SCROLL
				| SWT.V_SCROLL | SWT.CANCEL);

		Menu menu = new Menu(shell, SWT.BAR);
		shell.setMenuBar(menu);

		MenuItem mntmNewSubmenu = new MenuItem(menu, SWT.CASCADE);
		mntmNewSubmenu.setText("Data");

		Menu menu_1 = new Menu(mntmNewSubmenu);
		mntmNewSubmenu.setMenu(menu_1);

		MenuItem mntmSave = new MenuItem(menu_1, SWT.NONE);
		mntmSave.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				SaveFile s = new SaveFile(shell, SWT.DIALOG_TRIM);
				s.open(shell,styledText.getText());
			}
		});
		mntmSave.setText("save");
	}

	/**
	 * It changes the color of an char at the position x,y. For that it counts
	 * line breaks as y and character as x indices. The text starts with line 0
	 * and column 0 indices. If the position is out of bounds changeColor throws
	 * an Exception.
	 * 
	 * @param styledText
	 * @param shell
	 * @param x
	 * @param y
	 * @param r
	 * @param g
	 * @param b
	 * @throws Exception
	 */
	private static void changeColor(StyledText styledText, Shell shell, int x,
			int y, int red, int green, int blue) throws Exception {
		String text = styledText.getText();
		int lineBreakCounter = 0;
		int charAt = 0;// position of char in text which should be changed color
		// searching the right row
		for (; charAt < text.length() && lineBreakCounter != y; charAt++) {
			if (text.charAt(charAt) == '\n') {
				lineBreakCounter++;
			}
		}
		if (charAt + x > text.length() - 1) {
			throw new Exception("Char out of bounds!");
		}
		StyleRange styleRange = new StyleRange();
		styleRange.start = charAt + x;
		styleRange.length = 1;
		styleRange.fontStyle = SWT.BOLD;
		styleRange.foreground = new Color(shell.getDisplay(), red, green, blue);
		styledText.setStyleRange(styleRange);
	}
}
