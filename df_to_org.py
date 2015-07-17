from pandas import MultiIndex, DataFrame
from pandas.core.format import DataFrameFormatter

def sepjoin(sep, *lists):
    lengths = [max(map(len, x)) for x in lists]
    form = sep.join("{:<%d}" % l for l in lengths)
    tojoin = zip(*lists)
    return [form.format(*line) for line in tojoin]

class DFOrgFormatter(DataFrameFormatter):
    def _get_formatted_index_org(self, frame):
        # Taken from DataFrameFormatter to remove join of index
        index = frame.index
        columns = frame.columns

        show_index_names = self.show_index_names and self.has_index_names
        show_col_names = (self.show_index_names and self.has_column_names)

        fmt = self._get_formatter('__index__')

        if isinstance(index, MultiIndex):
            fmt_index = index.format(sparsify=self.sparsify, adjoin=False,
                                     names=show_index_names,
                                     formatter=fmt)
        else:
            fmt_index = [index.format(name=show_index_names, formatter=fmt)]
        fmt_index = [list(x) for x in fmt_index]
        # empty space for columns
        blank = [''] * columns.nlevels
        if show_col_names:
            col_header = ['%s' % x for x in self._get_column_name_list()]
        else:
            col_header = blank

        if self.header:
            fmt_index_1 = [blank + index for index in fmt_index[:-1]]
            fmt_index = fmt_index_1 + [col_header + fmt_index[-1]]
            return fmt_index
        else:
            return fmt_index

    def to_org(self):
        frame = self.frame
        buf = self.buf
        strcols = self._to_str_columns()[1:]

        if self.index:
            strcols = self._get_formatted_index_org(frame) + strcols

        lengths = [max(map(len, x)) for x in strcols]
        lines = sepjoin(" | ", *strcols)


        col_nlevels = frame.columns.nlevels
        if self.show_index_names and self.has_index_names:
            col_nlevels = col_nlevels + 1
        for i, row in enumerate(lines):
            if i == col_nlevels:
                hline = "|-" + "-+-".join("-"*l for l in lengths) + "-|\n"
                buf.write(hline)
                if self.index:
                    ind_nlevels = frame.index.nlevels
                    cols = [" "*l for l in lengths]
                    cols[0] = "/"+cols[0][1:]
                    cols[ind_nlevels] = "<"+cols[ind_nlevels][1:]
                    buf.write("| " + " | ".join(cols) + " |\n")
            buf.write("| " + row + " |\n")

def to_org(frame, **kwds):
    formatter = DFOrgFormatter(frame, **kwds)
    formatter.to_org()
    return formatter.buf.getvalue()

try:
    from IPython.core.formatters import BaseFormatter, Unicode, ObjectName
except ImportError:
    pass
else:
    class IPyOrgFormatter(BaseFormatter):
        format_type = Unicode('text/x-org')

        print_method = ObjectName('_repr_org_')

    def register_orgformatter():
        from IPython.core.interactiveshell import InteractiveShell
        formatter = InteractiveShell.instance().display_formatter
        orgformatter = IPyOrgFormatter(parent=formatter)
        formatter.formatters[orgformatter.format_type] = orgformatter
        formatter.active_types.append(orgformatter.format_type)
        orgformatter.for_type(DataFrame, to_org)
