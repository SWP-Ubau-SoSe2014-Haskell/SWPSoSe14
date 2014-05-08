package RailEditor;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;

public class FileDialog extends Dialog {

	protected Object result;
	protected Shell shell;
	private Text text;
	private String fileText;
	private StyledText styledText;
	private byte type;
	private static final int SHELLWIDTH = 395;
	private static final int SHELLHEIGHT = 69;
	public static final byte DIALOG_SAVE = 1;
	public static final byte DIALOG_OPEN = 0;

	/**
	 * Create the dialog.
	 * 
	 * @param parent
	 * @param style
	 */
	public FileDialog(Shell parent, int style) {
		super(parent, style);
		setText("SWT Dialog");
	}

	/**
	 * Open the dialog.
	 * 
	 * @return the result
	 */
	public Object open(Shell parent, StyledText styledText, byte type) {
		this.fileText = styledText.getText();
		this.type = type;
		this.styledText = styledText;
		createContents();
		Point parentLoc = parent.getLocation();
		Point parentSize = parent.getSize();
		parent.setEnabled(false);
		shell.setLocation(parentLoc.x + parentSize.x / 2 - SHELLWIDTH / 2,
				parentLoc.y - SHELLHEIGHT / 2 + parentSize.y / 2);
		shell.open();
		shell.layout();
		Display display = getParent().getDisplay();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
		parent.setEnabled(true);
		return result;
	}

	/**
	 * Create contents of the dialog.
	 */
	private void createContents() {
		shell = new Shell(getParent(), getStyle());

		shell.setSize(395, 69);
		shell.setText(getText());

		text = new Text(shell, SWT.BORDER);
		text.setBounds(62, 7, 223, 25);
		text.addKeyListener(new KeyAdapter() {
			public void keyPressed(KeyEvent e) {
				if (e.keyCode == 13)
					handleInput();
			}
		});

		Label lblFilepath = new Label(shell, SWT.NONE);
		lblFilepath.setBounds(1, 10, 55, 15);
		lblFilepath.setText("file path:");

		Button btnAction = new Button(shell, SWT.NONE);
		if (type == DIALOG_OPEN) {
			btnAction.setText("open");
		} else if (type == DIALOG_SAVE) {
			btnAction.setText("save");
		} else {
			shell.close();
		}
		btnAction.setBounds(304, 7, 75, 25);

		btnAction.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				handleInput();
			}
		});

	}

	private void handleInput() {
		String path = text.getText();
		File file = new File(path);
		try {
			if (type == DIALOG_OPEN) {
				char[] text = new char[(int) file.length()];
				FileReader reader = new FileReader(file);
				reader.read(text);
				reader.close();
				styledText.setText(String.valueOf(text));
			} else {
				FileWriter writer = new FileWriter(file);
				writer.write(fileText);
				writer.flush();
				writer.close();
			}
		} catch (IOException e1) {
			e1.printStackTrace();
			text.setText("Path does not exsists or access denied!");
			return;
		}
		shell.close();
	}
}
