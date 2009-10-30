#include <stdio.h>

#define TABLE_SIZE 4096
#define CHAR_COUNT 255

typedef unsigned char uchar;
struct entry {
	short  prefix;
	uchar suffix;
};

struct table {
	struct entry entries[TABLE_SIZE];
	short        size;
};

short table_insert(struct table *table, const struct entry e)
{
	short i = table->size;
	table->entries[i] = e;
	table->size++;
	return i;
}

void table_init(struct table *table)
{
	short i;
	table->size = 0;
	for (i = 0; i <= CHAR_COUNT; i++) {
		struct entry e;
		e.prefix = -1;
		e.suffix = (uchar) i;
		table_insert(table, e);
	}
}

short table_contains(const struct table *table, const struct entry e)
{
	short i;
	for (i = 0; i < table->size; i++) {
		const struct entry *entry = &table->entries[i];
		if (entry->prefix == e.prefix && entry->suffix == e.suffix) {
			return i;
		}
	}
	return -1;
}

void push_pattern(const short ptrn_idx, short *buf, int *offset)
{
	buf[(*offset)++] = ptrn_idx;
}

void push_string(const struct table *table, const short ptrn_idx,
		uchar *str, int *offset)
{
	const struct entry *e = &table->entries[ptrn_idx];
	if (e->prefix != -1) {
		push_string(table, e->prefix, str, offset);
	}
	str[(*offset)++] = e->suffix;
}

void table_print_pattern(const short ptrn_idx)
{
	if (('a' <= ptrn_idx && ptrn_idx <= 'z') ||
			('A' <= ptrn_idx && ptrn_idx <= 'Z') ||
			('0' <= ptrn_idx && ptrn_idx <= '9')) {
		printf("%c ", (uchar) ptrn_idx);
	} else {
		printf("<%d> ", ptrn_idx);
	}
	printf("\n");
}

void table_print_entry_helper(const struct table *table, const struct entry e)
{
	if (e.prefix != -1) {
		table_print_entry_helper(table, table->entries[e.prefix]);
	}
	if (('a' <= e.suffix && e.suffix <= 'z') ||
			('A' <= e.suffix && e.suffix <= 'Z') ||
			('0' <= e.suffix && e.suffix <= '9')) {
		printf("%c ", (int) e.suffix);
	} else {
		printf("char(%d)", (int) e.suffix);
	}
}

void table_print_entry(const struct table *table, const struct entry e)
{
	table_print_entry_helper(table, e);
	printf(" (at %d)\n", table_contains(table, e));
}

void compress(const uchar *s, short *ptrn_buf, int *ptrn_len)
{
	struct table table;
	const uchar *cp;
	short ptrn_idx;

	table_init(&table);

	cp = s;
	ptrn_idx = -1;
	*ptrn_len = 0;
	while (*cp) {
		short new_ptrn_idx;
		struct entry entry;

		entry.prefix = ptrn_idx;
		entry.suffix = *cp;
		if ((new_ptrn_idx = table_contains(&table, entry)) != -1) {
			ptrn_idx = new_ptrn_idx;
		} else {
			table_insert(&table, entry);
			push_pattern(ptrn_idx, ptrn_buf, ptrn_len);
			table_print_pattern(ptrn_idx);
			table_print_entry(&table, entry);
			ptrn_idx = (short) *cp;
		}
		cp++;
	}
	if (ptrn_idx != -1) {
		push_pattern(ptrn_idx, ptrn_buf, ptrn_len);
		table_print_pattern(ptrn_idx);
		table_print_entry(&table, table.entries[ptrn_idx]);
	}
}

uchar front(const struct table *table, short ptrn_idx)
{
	while (table->entries[ptrn_idx].prefix != -1)
		ptrn_idx = table->entries[ptrn_idx].prefix;
	return table->entries[ptrn_idx].suffix;
}

void decompress(const short *ptrn_buf, int ptrn_len, uchar *str)
{
	struct table table;
	short last, next;
	int i;
	int str_len = 0;

	if (ptrn_len == 0) {
		*str = '\0';
		return;
	}

	table_init(&table);

	last = ptrn_buf[0];
	push_string(&table, last, str, &str_len);
	table_print_entry(&table, table.entries[last]);
	for (i = 1; i < ptrn_len; i++) {
		struct entry e;
		e.prefix = last;
		next = ptrn_buf[i];
		e.suffix = front(&table, (next < table.size) ? next : last); 
		table_insert(&table, e);
		last = next;
		push_string(&table, last, str, &str_len);
		table_print_entry(&table, table.entries[last]);
	}
	str[str_len] = '\0';

}

int main(int argc, char **argv)
{
	const uchar *str = "LZWLZ78LZ77LZCLZMWLZAP";
	short ptrn_buf[512];
	uchar decompressed_str[512];
	int ptrn_len, str_len;
	int i;

	compress(str, ptrn_buf, &ptrn_len);
	printf("\n");
	printf("\n");
	printf("\n");
	for (i = 0; i < ptrn_len; i++) {
		table_print_pattern(ptrn_buf[i]);
	}
	printf("\n");
	printf("\n");
	printf("\n");
	printf("\n");
	decompress(ptrn_buf, ptrn_len, decompressed_str);
	printf("%s\n", str);
	printf("%s\n", decompressed_str);

	return 0;
}

