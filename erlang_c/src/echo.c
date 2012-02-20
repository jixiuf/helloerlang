//echo.c
#include <stdio.h>
#include <unistd.h>
typedef unsigned char byte;
typedef char int8;
int8 read_exact(byte* buf, int8 len);
int8 write_exact(byte* buf, int8 len);
int main() {
  FILE * fp;
  fp = fopen("ports.log", "w");
  fprintf(fp, "start...\n");
  fflush(fp);
  byte buf[256]={0};
  /* 读取一字节存在buf中，实际是存到buf[0],因为只读取了一字节 */
  /* 返回值为1表示成功读取了一字节 */
  while(read_exact(buf, 1)==1)
    {
      /* 因为约定，传过来的数据内容为：第一个字节表明，后绪内容的长度*/
      /* 这个过程是:首先读取第一个字节，假如读到的内容是10的话，则接下来的10个字节为需要一次读取的字节 */
      /* 即<<Len:8,Other/binary>> */
      int8 len = buf[0];
      /* 如果读取的的内容不够len那么长，则表明读取失败 */
      if(read_exact(buf, len)<=0) return -1;

      /* 如果走到这，证明，成功读取了后绪的len 字节
         注意前面用的是
         read_exact(buf, len),即从buf[0]开始存读取到的内容
         即buf[0]被覆盖了，其实长度已经存到变量len中了,此次存到buf中的数据才是
         我们真正想要的数据<<Other/binary>>
       */
      fprintf(fp, "buf:[%s],len:[%d]\n", buf, len);
      fflush(fp);
      /* 将len ,及buf内容写入到标准输出中 */
      /* &len是指向len的byte类型的指针,因为write_exact接受的参数类型是byte* */
      if(write_exact(&len, 1)<=0) return -1;
      /*  */
      if(write_exact(buf, len)<=0) return -1;
    }
  fprintf(fp, "end...\n");
  fclose(fp);
  return 0;
}
/* 从标准输入读取len 字节的内容存在buf中，并返回读取到的长度。如果标准输入的长度<len 则读多少算多少，
   并返回读取到的长度
 */
int8 read_exact(byte* buf, int8 len)
{
  int i, got=0;
  do {

    /* ssize_t read(int filedes, void *buf, size_t nbytes); */
    /* 返回值：读取到的字节数；0（读到 EOF）；-1（出错） */
    /* read 函数从 filedes 指定的已打开文件中读取 nbytes 字节到 buf 中。以下几种情况会导致读取到的字节数小于 nbytes ： */
    /* filedes: */
    /* The file descriptor of where to write the output. */
    /*   You can either use a file descriptor obtained from the open system call, or */
    /*   you can use 0, 1, or 2, to refer to standard input, standard output, or standard error, respectively. */
    /* read(0,..) */
      if ((i=read(0, buf+got, len-got)) <= 0)
        return (i);
    got += i;
  }while (got < len);
  return (len);
}
/* 将 buf中的内容写到标准输出中,最多写入len 字节，返回实际写进的字节长度。
   因为buf中字字节数可能比len 小
 */
int8 write_exact(byte* buf, int8 len)
{
  int i, wrote=0;
  do {
    if ((i= write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  }while (wrote < len);
  return (len);
}
