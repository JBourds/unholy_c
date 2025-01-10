int main(void) {
  goto start;
middle:
  goto end;
end:
  return 1;
start:
  goto middle;
}
