namespace Aardwars.Launcher
{
    using Aardwars;
    using System.Runtime.InteropServices;
    public partial class Form1 : Form
    {

        private void Form1_Load(object sender, EventArgs e)
        {
            AllocConsole();
            this.label1.Text = "My IPs:\n" + String.Join('\n', Elm.App.myIps);
        }

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool AllocConsole();

        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            Aardwars.MainGame.startGame("localhost", 7331);
        }

        private void doIt()
        {
            int port;
            if (System.Int32.TryParse(this.textBox2.Text, out port))
            {
                var server = this.textBox1.Text;
                Aardwars.MainGame.startGame(server, port);
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            doIt();
        }

        private void textBox1_KeyDown(object sender, KeyEventArgs e)
        {
            if(e.KeyCode== Keys.Enter)
            {
                doIt();
            }
        }
    }
}