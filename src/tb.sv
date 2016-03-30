module tb();

    logic clk, resetN, done, error, errorReg;
    logic [7:0] in;
    logic [1:0] TBerrorSelect;
    logic [7:0] data [18:0];

    loadMem lm (in, clk, resetN, TBerrorSelect, done, error);

    always #10 clk = ~clk;

    initial begin
        clk = 0;
        in = 0;
        resetN = 0;
        TBerrorSelect = 3;
        errorReg = 0;
        //$monitor("clk(%b) in(%h) done(%b) error(%b) errorReg(%b) TBerrorSelect(%b) mem[0](%h) mem[1](%h)", clk, in, done, error, errorReg, TBerrorSelect, lm.M.M[0], lm.M.M[1]);

        @(posedge clk) resetN = 0;
        @(posedge clk) resetN = 1;

        repeat(100) checkMem();

        @(posedge clk) $finish;

    end

    task checkMem();

        logic [7:0] data [18:0];
        logic [7:0] checksum, count, addr;
        logic [7:0] i;

        @(negedge clk) in = 8'h01;

        @(negedge clk) count = $urandom_range(19, 0);
        in = count;

        @(negedge clk) addr = $urandom_range(256-count, 0);
        in = addr;

        checksum = count + addr;

        for(i = 0; i < count; i++) begin
            @(negedge clk) data[i] = $urandom_range(255, 0);
            in = data[i];
            checksum += data[i];
            $display("checkMem: writing data(%h) to addr(%h)", data[i], addr+i);
        end

        @(negedge clk) in = -checksum;
        @(posedge clk) errorReg <= error;
        @(negedge clk) in = 0;

        for(i = 0; i < count; i++) begin
            if(lm.M.M[addr+i] != data[i])
                $display("checkMem: ERROR addr(%h) is (%h), expected (%h)", addr+i, lm.M.M[addr+i], data[i]);
            else
                $display("checkMem: PASS addr(%h) is (%h), expected (%h)", addr+i, lm.M.M[addr+i], data[i]);
        end

    endtask

endmodule


