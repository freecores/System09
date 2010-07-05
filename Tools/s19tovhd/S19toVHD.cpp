// S19toVHD.cpp : Defines the entry point for the console application.
//

/*
* epedit
*
* binary file editer program
*/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
/*
* equates
*/
#define EPROM_MAX (1<<16)
#define CMD_LINE_MAX 80
#define FALSE 0
#define TRUE !FALSE
#define BINARY 0
#define MOTOROLA 1
#define INTEL 2
#define SMAL32 3
#define VHDL_BIN 4
#define VHDL_BYTE 5
#define VHDL_WORD 6

/*
* global variables
*/
FILE *cmdfp;				/* command input pointer */
char cmdbuff[CMD_LINE_MAX];
unsigned char eprom_buff[EPROM_MAX];	/* eprom buffer */
int eprom_top;				/* top of EPROM buffer */
int mod_flag;				/* buffer has been modified */
int auxflag;				/* Auxillary input file specified */
int count;
int checksum;
int offset;				/* Eprom Buffer memory offset */
int format_type;			/* load / save format type */
char *hex_str = "0123456789ABCDEF";


/*
* compare a string of specified length
* return TRUE if a match
* return FALSE if no match
* ignore case
*/
int str_equal( char *s1, char *s2, int len )
{
	int i;

	i = 0;
	while( i<len )
	{
		if( toupper( s1[i] ) == toupper( s2[i] ) )
			i++;
		else return FALSE;
	}
	return TRUE;
}	


int to_hexadecimal( char c )
{
	int k;

	for( k=0; k<16; k++ )
	{
		if( toupper(c) == hex_str[k] )
			return k;
	}
	return -1;
}			

/*
* extract an address from the command line
* returns an offset to the end of the argument.
*/
int get_address( char *cb, int *addr )
{
	int i, j, k;

	j = 0;
	i = 0;

	while((k = to_hexadecimal(cb[i])) != -1)
	{
		i++;
		j = j *16 + k;
	}
	*addr = j;
	if( i == 0 )
		return i;
	while( isspace( cb[i]) )
		i++;
	return i;
}


/*
* Motorola S1 format to Intel hex format
* Usage
* mot2hex <file_name>
*/

int gethex( FILE *fp_in )
{
	int hex;

	hex = fgetc( fp_in );
	return( to_hexadecimal( hex ) );
}

int get2hex( FILE *fp_in )
{
	int hexhi, hexlo, byte;

	hexhi = gethex( fp_in );
	if( hexhi != -1 )
	{
		hexlo = gethex( fp_in );
		if( hexlo != -1 )
		{
			byte = hexhi * 16 + hexlo;
			checksum = (checksum + byte) & 0xff;
			return byte;
		}
	}
	return -1;
}

int get4hex( FILE *fp_in )
{
	int bytehi, bytelo, addr;

	bytehi = get2hex( fp_in );
	if( bytehi != -1 )
	{
		bytelo = get2hex( fp_in );
		if( bytelo != -1 )
		{
			addr = (bytehi * 256) + bytelo;
			return addr;
		}
	}
	return -1;
}    

int get6hex( FILE *fp_in )
{
	int bytehi, bytemid, bytelow, addr;

	bytehi = get2hex( fp_in );
	if( bytehi != -1 )
	{
		bytemid = get2hex( fp_in );
		if( bytemid != -1 )
		{
			bytelow = get2hex( fp_in );
			if( bytelow != -1 )
			{
				addr = (bytehi << 16) + (bytemid << 8) + bytelow;
				return addr;
			}
		}
	}
	return -1;
}    

long get8hex( FILE *fp_in )
{
	int wordhi, wordlow;
	long addr;

	wordhi = get4hex( fp_in );
	if( wordhi != -1 )
	{
		wordlow = get4hex( fp_in );
		if( wordlow != -1 )
		{
			addr = ((long)wordhi << 16) + (long)wordlow;
			return addr;
		}
	}
	return -1;
}    




/*
* load motorola formatted file
*/

bool load_mot( char *fname_in )
{
	FILE *fp_in;
	int byte, addr, i;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
		printf( "\nCan't open %s", fname_in );
		return false; 
	}

	byte = 0;
	addr = 0;

	while( byte != -1 )
	{
		do {
			byte = fgetc( fp_in);
		} while( (byte != 'S') && (byte != -1) );

		byte = fgetc( fp_in );
		checksum = 0;
		if( (byte == '1') || (byte == '2') )
		{
			count = get2hex( fp_in );
			if( byte == '1' )
			{
				addr = get4hex( fp_in );
				count -= 3;
			}
			else
			{
				addr = get6hex( fp_in );
				count -= 4;
			}
			for( i=0; i<count; i++ )
			{
				byte = get2hex( fp_in );
				eprom_buff[( addr - offset) % EPROM_MAX ] = (unsigned char)byte;
				addr++;
			}
			byte = get2hex( fp_in);
			checksum = (~checksum) & 0xff;
			if( checksum != 0 )
				printf( "\nchecksum error - read check = %02x", byte );
		}
	}
	fclose( fp_in );
	return true;
}




int put2hex( FILE *fp, int h )
{
	int i, hex;

	hex = (h & 0xf0)>>4;
	i = fputc( (int)hex_str[hex], fp );
	hex = (h & 0xf);
	i = fputc( (int)hex_str[hex], fp );
	checksum = (checksum + h) & 0xff;
	return i;
}

int put4hex( FILE * fp, int h )
{
	int i;

	i = put2hex( fp, (h & 0xff00 )>>8 );
	i = put2hex( fp, (h & 0xff) );
	return i;
}


/*
* save VHDL hexadecimal file
*/

void save_vhdl_byte( FILE *fp_out, char *entity_name, int start_addr, int end_addr )
{
	int addr;
	int i,j;
	int byte;

	j=0;
	fprintf(fp_out, "library IEEE;\n");
	fprintf(fp_out, "   use IEEE.std_logic_1164.all;\n");
	fprintf(fp_out, "   use IEEE.std_logic_arith.all;\n");
	fprintf(fp_out, "library unisim;\n");
	fprintf(fp_out, "   use unisim.vcomponents.all;\n");
	fprintf(fp_out, "\n");
	fprintf(fp_out, "entity %s is\n", entity_name);
	fprintf(fp_out, "   port(\n");
	fprintf(fp_out, "      clk    : in  std_logic;\n");
	fprintf(fp_out, "      rst    : in  std_logic;\n");
	fprintf(fp_out, "      cs     : in  std_logic;\n");
	fprintf(fp_out, "      rw     : in  std_logic;\n");
	fprintf(fp_out, "      addr   : in  std_logic_vector(10 downto 0);\n");
	fprintf(fp_out, "      rdata  : out std_logic_vector(7 downto 0);\n");
	fprintf(fp_out, "      wdata  : in  std_logic_vector(7 downto 0)\n");
	fprintf(fp_out, "   );\n");
	fprintf(fp_out, "end %s;\n", entity_name);
	fprintf(fp_out, "\n");
	fprintf(fp_out, "architecture rtl of %s is\n", entity_name);
	fprintf(fp_out, "   signal we : std_logic;\n");
	fprintf(fp_out, "   signal dp : std_logic;\n");
	fprintf(fp_out, "begin\n");
	fprintf(fp_out, "   ROM: RAMB16_S9\n");
	fprintf(fp_out, "      generic map (\n");

	for( addr=start_addr; addr<=end_addr; addr+=32 )
	{
		fprintf( fp_out, "         INIT_%02x => x\"", j );
		for(i=31; i>=0; i-- )
		{
			byte = (int)eprom_buff[(addr - offset + i) % EPROM_MAX];
			putc( hex_str[(byte >>4) & 0xf], fp_out ); 
			putc( hex_str[byte & 0xf], fp_out ); 
		}
		if (addr+32 < end_addr) {
			fprintf( fp_out, "\",\n" );
		} else {
			fprintf( fp_out, "\"\n" );
		}
		j++;
	}
	fprintf(fp_out, "      )\n");
	fprintf(fp_out, "      port map (\n");
	fprintf(fp_out, "         do    => rdata,\n");
	fprintf(fp_out, "         dop(0)  => dp,\n");
	fprintf(fp_out, "         addr    => addr,\n");
	fprintf(fp_out, "         clk     => clk,\n");
	fprintf(fp_out, "         di      => wdata,\n");
	fprintf(fp_out, "         dip(0)  => dp,\n");
	fprintf(fp_out, "         en      => cs,\n");
	fprintf(fp_out, "         ssr     => rst,\n");
	fprintf(fp_out, "         we      => we\n");
	fprintf(fp_out, "      );\n");
	fprintf(fp_out, "   drive_we: process (rw)\n");
	fprintf(fp_out, "   begin\n");
	fprintf(fp_out, "      we <= not rw;\n");
	fprintf(fp_out, "   end process;\n");
	fprintf(fp_out, "end architecture rtl;\n\n");

}




/*
* epedit main program
*/
int main(int argc, char* argv[])
{
	int start_addr;
	int end_addr;
	int arglen;
	char entity_name_buf[512];
	char hdl_file_buf[1024];
	char buf[1024];
	char *curpos;
	FILE *fp_out;

	if (argc < 5) {
		printf("Usage: s19tovhd <s19 file> <base vhd file> <vhdl base entity name> <addr> [<addr> ...]\n");
		return(-1);
	}
	printf("Reading Motorola S19 from file '%s'\n", argv[1]);
	printf("VHDL file name '%s'\n", argv[2]);
	printf("Base RAM/ROM entity name is '%s'\n", argv[3]);
	if (!load_mot( argv[1] )) {
		return(-1);
        }
	if( (fp_out = fopen( argv[2], "w" )) == NULL ) {
		printf( "\nCan't open '%s' for write ", argv[2] );
		return(-1);
	}

	for (int cnt=4; cnt<argc; cnt++) {
		if( (arglen = get_address( argv[cnt], &start_addr )) == 0 ) {
			printf("Expected hex start address, got %s\n", argv[cnt]);
			continue;
		}
		end_addr = start_addr + 2047;
		sprintf(entity_name_buf, "%s_%4X", argv[3], start_addr);

		printf("Entity '%s' (address range '0x%4X'-'0x%4X') written to file '%s'\n", 
			entity_name_buf, start_addr, end_addr, argv[2]);
		save_vhdl_byte( fp_out, entity_name_buf, start_addr, end_addr );
	}
	if (fp_out) fclose(fp_out);
	return(0);
}

