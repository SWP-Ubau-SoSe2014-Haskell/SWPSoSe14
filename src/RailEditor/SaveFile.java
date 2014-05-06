package RailEditor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;

public class SaveFile extends Dialog {

	protected Object result;
	protected Shell shell;
	private Text text;
	private String fileText;
	private Shell parent;
	private static final int SHELLWIDTH = 395;
	private static final int SHELLHEIGHT = 69;

	/**
	 * Create the dialog.
	 * 
	 * @param parent
	 * @param style
	 */
	public SaveFile(Shell parent, int style) {
		super(parent, style);
		setText("SWT Dialog");
	}

	/**
	 * Open the dialog.
	 * 
	 * @return the result
	 */
	public Object open(Shell parent, String fileText) {
		this.fileText = fileText;
		this.parent = parent;
		createContents();
		Point parentLoc = parent.getLocation();
		Point parentSize = parent.getSize();
		parent.setEnabled(false);
		shell.setLocation(parentLoc.x + parentSize.x / 2-SHELLWIDTH/2, parentLoc.y-SHELLHEIGHT/2
				+ parentSize.y / 2);
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

		Label lblFilepath = new Label(shell, SWT.NONE);
		lblFilepath.setBounds(1, 10, 55, 15);
		lblFilepath.setText("file path:");

		Button btnSave = new Button(shell, SWT.NONE);
		btnSave.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				String path = text.getText();
				File file = new File(path);

				try {
					FileWriter writer = new FileWriter(file);
					writer.write(fileText);
					writer.flush();
					writer.close();
				} catch (IOException e1) {
					e1.printStackTrace();
					text.setText("Path does not exsists or access denied!");
					return;
				}
				shell.close();
			}
		});
		btnSave.setBounds(304, 7, 75, 25);
		btnSave.setText("save");

	}
}
