namespace TestApplication
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.btn4ModulesAndVariables = new System.Windows.Forms.Button();
            this.btn4TestMatrices = new System.Windows.Forms.Button();
            this.btn4PVWattsFunc = new System.Windows.Forms.Button();
            this.btn4PVWatts = new System.Windows.Forms.Button();
            this.btn4ModuleList = new System.Windows.Forms.Button();
            this.btn4Version = new System.Windows.Forms.Button();
            this.txtData = new System.Windows.Forms.RichTextBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.btnCashLoan = new System.Windows.Forms.Button();
            this.btnPVCom = new System.Windows.Forms.Button();
            this.btnBelpe = new System.Windows.Forms.Button();
            this.btnPVSamV1 = new System.Windows.Forms.Button();
            this.btn4ArrayTest = new System.Windows.Forms.Button();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // btn4ModulesAndVariables
            // 
            this.btn4ModulesAndVariables.Location = new System.Drawing.Point(212, 34);
            this.btn4ModulesAndVariables.Name = "btn4ModulesAndVariables";
            this.btn4ModulesAndVariables.Size = new System.Drawing.Size(137, 23);
            this.btn4ModulesAndVariables.TabIndex = 15;
            this.btn4ModulesAndVariables.Text = "Modules and Variables";
            this.btn4ModulesAndVariables.UseVisualStyleBackColor = true;
            this.btn4ModulesAndVariables.Click += new System.EventHandler(this.btn4ModulesAndVariables_Click);
            // 
            // btn4TestMatrices
            // 
            this.btn4TestMatrices.Location = new System.Drawing.Point(436, 34);
            this.btn4TestMatrices.Name = "btn4TestMatrices";
            this.btn4TestMatrices.Size = new System.Drawing.Size(95, 23);
            this.btn4TestMatrices.TabIndex = 14;
            this.btn4TestMatrices.Text = "Test Matrices";
            this.btn4TestMatrices.UseVisualStyleBackColor = true;
            this.btn4TestMatrices.Click += new System.EventHandler(this.btn4TestMatrices_Click);
            // 
            // btn4PVWattsFunc
            // 
            this.btn4PVWattsFunc.Location = new System.Drawing.Point(113, 65);
            this.btn4PVWattsFunc.Name = "btn4PVWattsFunc";
            this.btn4PVWattsFunc.Size = new System.Drawing.Size(110, 23);
            this.btn4PVWattsFunc.TabIndex = 13;
            this.btn4PVWattsFunc.Text = "PVWatts Func";
            this.btn4PVWattsFunc.UseVisualStyleBackColor = true;
            this.btn4PVWattsFunc.Click += new System.EventHandler(this.btn4PVWattsFunc_Click);
            // 
            // btn4PVWatts
            // 
            this.btn4PVWatts.Location = new System.Drawing.Point(32, 65);
            this.btn4PVWatts.Name = "btn4PVWatts";
            this.btn4PVWatts.Size = new System.Drawing.Size(75, 23);
            this.btn4PVWatts.TabIndex = 12;
            this.btn4PVWatts.Text = "PVWatts Example";
            this.btn4PVWatts.UseVisualStyleBackColor = true;
            this.btn4PVWatts.Click += new System.EventHandler(this.btn4PVWatts_Click);
            // 
            // btn4ModuleList
            // 
            this.btn4ModuleList.Location = new System.Drawing.Point(111, 34);
            this.btn4ModuleList.Name = "btn4ModuleList";
            this.btn4ModuleList.Size = new System.Drawing.Size(95, 23);
            this.btn4ModuleList.TabIndex = 11;
            this.btn4ModuleList.Text = "Module List";
            this.btn4ModuleList.UseVisualStyleBackColor = true;
            this.btn4ModuleList.Click += new System.EventHandler(this.btn4ModuleList_Click);
            // 
            // btn4Version
            // 
            this.btn4Version.Location = new System.Drawing.Point(30, 34);
            this.btn4Version.Name = "btn4Version";
            this.btn4Version.Size = new System.Drawing.Size(75, 23);
            this.btn4Version.TabIndex = 10;
            this.btn4Version.Text = "Version";
            this.btn4Version.UseVisualStyleBackColor = true;
            this.btn4Version.Click += new System.EventHandler(this.btn4Version_Click);
            // 
            // txtData
            // 
            this.txtData.Location = new System.Drawing.Point(30, 94);
            this.txtData.Name = "txtData";
            this.txtData.Size = new System.Drawing.Size(762, 528);
            this.txtData.TabIndex = 9;
            this.txtData.Text = "";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.btnCashLoan);
            this.groupBox2.Controls.Add(this.btnPVCom);
            this.groupBox2.Controls.Add(this.btnBelpe);
            this.groupBox2.Controls.Add(this.btnPVSamV1);
            this.groupBox2.Controls.Add(this.btn4PVWattsFunc);
            this.groupBox2.Controls.Add(this.btn4ModulesAndVariables);
            this.groupBox2.Controls.Add(this.btn4PVWatts);
            this.groupBox2.Controls.Add(this.btn4ArrayTest);
            this.groupBox2.Controls.Add(this.btn4TestMatrices);
            this.groupBox2.Controls.Add(this.txtData);
            this.groupBox2.Controls.Add(this.btn4Version);
            this.groupBox2.Controls.Add(this.btn4ModuleList);
            this.groupBox2.Location = new System.Drawing.Point(12, 23);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(821, 646);
            this.groupBox2.TabIndex = 16;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "SSC API c# test";
            // 
            // btnCashLoan
            // 
            this.btnCashLoan.Location = new System.Drawing.Point(458, 65);
            this.btnCashLoan.Name = "btnCashLoan";
            this.btnCashLoan.Size = new System.Drawing.Size(75, 23);
            this.btnCashLoan.TabIndex = 19;
            this.btnCashLoan.Text = "Cash Loan";
            this.btnCashLoan.UseVisualStyleBackColor = true;
            this.btnCashLoan.Click += new System.EventHandler(this.btnCashLoan_Click);
            // 
            // btnPVCom
            // 
            this.btnPVCom.Location = new System.Drawing.Point(365, 63);
            this.btnPVCom.Name = "btnPVCom";
            this.btnPVCom.Size = new System.Drawing.Size(87, 23);
            this.btnPVCom.TabIndex = 18;
            this.btnPVCom.Text = "Utility Rate 3";
            this.btnPVCom.UseVisualStyleBackColor = true;
            this.btnPVCom.Click += new System.EventHandler(this.btnUtilityRate3_Click);
            // 
            // btnBelpe
            // 
            this.btnBelpe.Location = new System.Drawing.Point(312, 63);
            this.btnBelpe.Name = "btnBelpe";
            this.btnBelpe.Size = new System.Drawing.Size(47, 23);
            this.btnBelpe.TabIndex = 17;
            this.btnBelpe.Text = "Belpe";
            this.btnBelpe.UseVisualStyleBackColor = true;
            this.btnBelpe.Click += new System.EventHandler(this.btnBelpe_Click);
            // 
            // btnPVSamV1
            // 
            this.btnPVSamV1.Location = new System.Drawing.Point(230, 64);
            this.btnPVSamV1.Name = "btnPVSamV1";
            this.btnPVSamV1.Size = new System.Drawing.Size(75, 23);
            this.btnPVSamV1.TabIndex = 16;
            this.btnPVSamV1.Text = "PV Sam V1";
            this.btnPVSamV1.UseVisualStyleBackColor = true;
            this.btnPVSamV1.Click += new System.EventHandler(this.btnPVSamV1_Click);
            // 
            // btn4ArrayTest
            // 
            this.btn4ArrayTest.Location = new System.Drawing.Point(355, 34);
            this.btn4ArrayTest.Name = "btn4ArrayTest";
            this.btn4ArrayTest.Size = new System.Drawing.Size(75, 23);
            this.btn4ArrayTest.TabIndex = 6;
            this.btn4ArrayTest.Text = "Test Arrays";
            this.btn4ArrayTest.UseVisualStyleBackColor = true;
            this.btn4ArrayTest.Click += new System.EventHandler(this.btn4ArrayTest_Click);
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(840, 693);
            this.Controls.Add(this.groupBox2);
            this.Name = "Form1";
            this.Text = "SSC Test Application";
            this.groupBox2.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button btn4ModulesAndVariables;
        private System.Windows.Forms.Button btn4TestMatrices;
        private System.Windows.Forms.Button btn4PVWattsFunc;
        private System.Windows.Forms.Button btn4PVWatts;
        private System.Windows.Forms.Button btn4ModuleList;
        private System.Windows.Forms.Button btn4Version;
        private System.Windows.Forms.RichTextBox txtData;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Button btn4ArrayTest;
        private System.Windows.Forms.Button btnPVSamV1;
        private System.Windows.Forms.Button btnBelpe;
        private System.Windows.Forms.Button btnPVCom;
        private System.Windows.Forms.Button btnCashLoan;
    }
}

