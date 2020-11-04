import javax.swing.*;
import java.awt.*;

public class GUI extends JFrame {

    private JLabel nPlayerLabel;
    private JLabel nNumberLabel;
    private JTextField nPlayerText;
    private JTextField nNumberText;
    private JButton startButton;
    private JPanel startPanel;

    public GUI(final int w, final int h) {
        setTitle("MasterMind GUI");
        setSize(w, h);
        JFrame myFrame = new JFrame();
        myFrame.setSize(new Dimension(w, h));
        myFrame.setResizable(false);


        startPanel = new JPanel();
        JPanel nPlayerPanel = new JPanel();
        JPanel nNumberPanel = new JPanel();
        JPanel buttonPanel = new JPanel();

        SpringLayout layout = new SpringLayout();
        startPanel.setLayout(layout);
        startPanel.add(nPlayerPanel);
        startPanel.add(nNumberPanel);
        startPanel.add(buttonPanel);

        layout.putConstraint(SpringLayout.NORTH, nPlayerPanel, 0, SpringLayout.NORTH, startPanel);
        layout.putConstraint(SpringLayout.WEST, nPlayerPanel, 0, SpringLayout.WEST, startPanel);

        layout.putConstraint(SpringLayout.WEST, nNumberPanel, 0, SpringLayout.WEST, startPanel);

        layout.putConstraint(SpringLayout.SOUTH, nNumberPanel, 0, SpringLayout.NORTH, buttonPanel);

        layout.putConstraint(SpringLayout.WEST, buttonPanel, 0, SpringLayout.WEST, startPanel);
        layout.putConstraint(SpringLayout.SOUTH, buttonPanel, 0, SpringLayout.SOUTH, startPanel);

        nPlayerLabel = new JLabel("Choose a players number", JLabel.TRAILING);
        nPlayerText = new JTextField(10);
        nPlayerLabel.setLabelFor(nPlayerText);
        nNumberLabel = new JLabel("Choose how much numbers you have to guess", JLabel.TRAILING);
        nNumberText = new JTextField(10);
        nNumberLabel.setLabelFor(nNumberText);
        startButton = new JButton("START");

        nPlayerPanel.add(nPlayerLabel);
        nPlayerPanel.add(nPlayerText);
        nNumberPanel.add(nNumberLabel);
        nNumberPanel.add(nNumberText);
        buttonPanel.add(startButton);

        myFrame.add(startPanel);
        myFrame.setLocationRelativeTo(null);
        myFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        myFrame.setVisible(true);
    }

    private void startGame() {

    }
}
