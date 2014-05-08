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
import org.eclipse.wb.swt.SWTResourceManager;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.swt.widgets.CoolBar;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.custom.TableTree;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.jface.text.TextViewer;

public class Gui {

	protected Shell shell;
	private StyledText styledText;
	private final FormToolkit formToolkit = new FormToolkit(Display.getDefault());

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
		shell.setSize(573, 450);
		shell.setBounds(
				Toolkit.getDefaultToolkit().getScreenSize().width / 2 - 450 / 2,
				Toolkit.getDefaultToolkit().getScreenSize().height / 2 - 300 / 2,
				450, 300);
		shell.setText("SWT Application");
		shell.setLayout(new RowLayout(SWT.HORIZONTAL));
		//shell.setLayout(new FillLayout(SWT.HORIZONTAL));

		styledText = new StyledText(shell, SWT.BORDER);
		styledText.setLayoutData(new RowData(246, 214));
		styledText.setFont(SWTResourceManager.getFont("Courier New", 9,
				SWT.NORMAL));

		Menu menu = new Menu(shell, SWT.BAR);
		shell.setMenuBar(menu);

		MenuItem mntmNewSubmenu = new MenuItem(menu, SWT.CASCADE);
		mntmNewSubmenu.setText("Data");

		Menu menu_1 = new Menu(mntmNewSubmenu);
		mntmNewSubmenu.setMenu(menu_1);

		MenuItem mntmOpenFile = new MenuItem(menu_1, SWT.NONE);
		mntmOpenFile.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				FileDialog s = new FileDialog(shell, SWT.DIALOG_TRIM);
				s.open(shell, styledText, FileDialog.DIALOG_OPEN);
			}
		});
		mntmOpenFile.setText("open file");

		MenuItem mntmSave = new MenuItem(menu_1, SWT.NONE);
		mntmSave.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				FileDialog s = new FileDialog(shell, SWT.DIALOG_TRIM);
				s.open(shell, styledText, FileDialog.DIALOG_SAVE);
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
