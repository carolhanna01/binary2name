#ifndef ITEMMODEL_H_
#define ITEMMODEL_H_

#include <QStandardItemModel>
#include <QAbstractItemModel>
#include <QMutex>

class GItemModel : public QStandardItemModel
{
  Q_OBJECT

public:
  explicit GItemModel(QObject *parent = 0);
  virtual ~GItemModel();

  bool hasIndex(int row, int column, const QModelIndex &parent = QModelIndex()) const;
  virtual QModelIndex index(int row, int column,
                            const QModelIndex &parent = QModelIndex()) const;
  virtual QModelIndex parent(const QModelIndex &child) const;

  inline QModelIndex sibling(int row, int column, const QModelIndex &idx) const
      { return index(row, column, parent(idx)); }

  virtual int rowCount(const QModelIndex &parent = QModelIndex()) const;
  virtual int columnCount(const QModelIndex &parent = QModelIndex()) const;
  virtual bool hasChildren(const QModelIndex &parent = QModelIndex()) const;

  virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
  virtual bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole);

  virtual QVariant headerData(int section, Qt::Orientation orientation,
                              int role = Qt::DisplayRole) const;
  virtual bool setHeaderData(int section, Qt::Orientation orientation, const QVariant &value,
                             int role = Qt::EditRole);

  virtual QMap<int, QVariant> itemData(const QModelIndex &index) const;
  virtual bool setItemData(const QModelIndex &index, const QMap<int, QVariant> &roles);

  virtual QStringList mimeTypes() const;
  virtual QMimeData *mimeData(const QModelIndexList &indexes) const;
  virtual bool dropMimeData(const QMimeData *data, Qt::DropAction action,
                            int row, int column, const QModelIndex &parent);
  virtual Qt::DropActions supportedDropActions() const;

  Qt::DropActions supportedDragActions() const;
  void setSupportedDragActions(Qt::DropActions);

  virtual bool insertRows(int row, int count, const QModelIndex &parent = QModelIndex());
  virtual bool insertColumns(int column, int count, const QModelIndex &parent = QModelIndex());
  virtual bool removeRows(int row, int count, const QModelIndex &parent = QModelIndex());
  virtual bool removeColumns(int column, int count, const QModelIndex &parent = QModelIndex());

  bool insertRow(int row, const QModelIndex &parent = QModelIndex());
  bool insertColumn(int column, const QModelIndex &parent = QModelIndex());
  bool removeRow(int row, const QModelIndex &parent = QModelIndex());
  bool removeColumn(int column, const QModelIndex &parent = QModelIndex());

  virtual void fetchMore(const QModelIndex &parent);
  virtual bool canFetchMore(const QModelIndex &parent) const;
  virtual Qt::ItemFlags flags(const QModelIndex &index) const;
  virtual void sort(int column, Qt::SortOrder order = Qt::AscendingOrder);
  virtual QModelIndex buddy(const QModelIndex &index) const;
  virtual QModelIndexList match(const QModelIndex &start, int role,
                                const QVariant &value, int hits = 1,
                                Qt::MatchFlags flags =
                                Qt::MatchFlags(Qt::MatchStartsWith|Qt::MatchWrap)) const;
  virtual QSize span(const QModelIndex &index) const;

  QObject *parent() const { return QObject::parent(); }

  void setColumnCount(int columns);
  QStandardItem *itemFromIndex(const QModelIndex &index) const;
  QStandardItem *invisibleRootItem() const;
  QStandardItem *item(int row, int column = 0) const;
  void clear();
  void appendRow(QStandardItem *item);

  void lock();
  void unlock();

  QAbstractItemModel *abstractItemModel();
  QObject *object();

private: // can only be emitted by QAbstractItemModel
    void rowsAboutToBeInserted(const QModelIndex &parent, int first, int last);
    void rowsInserted(const QModelIndex &parent, int first, int last);

    void rowsAboutToBeRemoved(const QModelIndex &parent, int first, int last);
    void rowsRemoved(const QModelIndex &parent, int first, int last);

    void columnsAboutToBeInserted(const QModelIndex &parent, int first, int last);
    void columnsInserted(const QModelIndex &parent, int first, int last);

    void columnsAboutToBeRemoved(const QModelIndex &parent, int first, int last);
    void columnsRemoved(const QModelIndex &parent, int first, int last);

    void modelAboutToBeReset();
    void modelReset();

    QMutex *modelLock;
protected:
    QModelIndex createIndex(int row, int column, void *data = 0) const;
    QModelIndex createIndex(int row, int column, int id) const;
    QModelIndex createIndex(int row, int column, quint32 id) const;

    void encodeData(const QModelIndexList &indexes, QDataStream &stream) const;
    bool decodeData(int row, int column, const QModelIndex &parent, QDataStream &stream);

    void beginInsertRows(const QModelIndex &parent, int first, int last);
    void endInsertRows();

    void beginRemoveRows(const QModelIndex &parent, int first, int last);
    void endRemoveRows();

    void beginInsertColumns(const QModelIndex &parent, int first, int last);
    void endInsertColumns();

    void beginRemoveColumns(const QModelIndex &parent, int first, int last);
    void endRemoveColumns();

    void reset();

    void changePersistentIndex(const QModelIndex &from, const QModelIndex &to);
    void changePersistentIndexList(const QModelIndexList &from, const QModelIndexList &to);
    QModelIndexList persistentIndexList() const;
};

#endif /*ITEMMODEL_H_*/
